package net.noresttherein.sugar.collection

import scala.collection.immutable.ArraySeq

import org.scalacheck.Prop._
import org.scalacheck.Properties




object ReverseArrayIteratorSpec extends Properties("ReverseArrayIterator") {
	private val array = (0 to Short.MaxValue) to Array

	property("knownSize") = forAll { (from :Short, until :Short) =>
		val it = new ReverseArrayIterator(array, from, until)
		it.knownSize ?= ((until max from max 0 min Short.MaxValue) - (from max 0 min Short.MaxValue))
	}
	property("size") = forAll { (from :Short, until :Short) =>
		val it = new ReverseArrayIterator(array, from, until)
		it.knownSize ?= ((until max from max 0 min Short.MaxValue) - (from max 0 min Short.MaxValue))
	}
	property("head") = forAll { i :Short =>
		val idx = i min Short.MaxValue - 1 max 0
		val it = new ReverseArrayIterator(array, idx, idx + 1)
		it.head ?= array(idx)
	}
	property("next") = forAll { (from :Short, until :Short) =>
		val it = new ReverseArrayIterator(array, from, until)
		(it.to(ArraySeq) ?= ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse) :| s"[$from, $until)"
	}

	property("take") = forAll { (from :Short, until :Short, take :Int) =>
		val it = new ReverseArrayIterator(array, from, until).take(take)
		(it.to(ArraySeq) ?= array.view.slice(from, until).reverse.take(take).to(ArraySeq)) :| s"[$from, $until) take $take"
	}
	property("drop") = forAll { (from :Short, until :Short, drop :Int) =>
		val it = new ReverseArrayIterator(array, from, until).drop(drop)
		(it.to(ArraySeq) ?= array.view.slice(from, until).reverse.drop(drop).to(ArraySeq)) :| s"[$from, $until) drop $drop"
	}
	property("slice") = forAll { (from :Short, until :Short, start :Short, end :Short) =>
		val result = new ReverseArrayIterator(array, from, until).slice(start, end) to ArraySeq
		val expect = array.view.slice(from, until).reverse.slice(start, end) to ArraySeq
		(result ?= expect) :| s"[$from, $until).slice($start, $end)"
	}
	property("splitAt") = forAll { (from :Short, until :Short, split :Short) =>
		val (first, second) = new ReverseArrayIterator(array, from, until).splitAt(split)
		val expect = ArraySeq.unsafeWrapArray(array).slice(from, until).reverse.splitAt(split)
		(first to ArraySeq, second to ArraySeq) ?= expect
	}
}
