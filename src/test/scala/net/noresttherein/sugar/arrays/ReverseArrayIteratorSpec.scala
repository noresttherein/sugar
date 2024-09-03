package net.noresttherein.sugar.arrays

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Prop, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter

import net.noresttherein.sugar.collections.ValIterator
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, Prettify}
import net.noresttherein.sugar.witness.NullValue




//todo: move it to the file with ArrayIteratorSpec
object ReverseArrayIteratorSpec extends ArrayTestingUtils("ReverseArrayIterator") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(200)

	//The public iterator type returned by ReverseArrayIterator factory object.
	type TestedIterator[+A] = ValIterator.Buffered[A]

	property("ReverseArrayIterator.apply") = forAll { (array :Array[Int], start :Int, length :Int) =>
		val iter = ReverseArrayIterator(array, start, length)
		val expect = array.take(math.min(start, array.length - 1) + 1).reverseIterator.take(length)
		iter sameElements expect lbl
			"ReverseArrayIterator(" + array.contentsString + ", " + start + ", " + length + ") ==" +
				ReverseArrayIterator(array, start, length).mkString("Iterator(", ", ", ")")
	}

	private abstract class ReverseArrayIteratorProperty(name :String) extends ArrayProperty(name) {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0  = from max 0 min array.length
				val until0 = until max from0 min array.length
				apply(array, from0, until0, ReverseArrayIterator.slice(array, from, until))
			}

		def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop
	}

	new ReverseArrayIteratorProperty("knownSize") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
			iterator.knownSize ?= (until - from)
	}
	new ReverseArrayIteratorProperty("size") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
			iterator.size ?= (until - from)
	}
	new ReverseArrayIteratorProperty("head") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
			if (until <= from)
				iterator.head.throws[NoSuchElementException]
			else if (until == from + 1)
				iterator.head ?= array(until - 1)
			else
				(iterator.head ?= array(until - 1)) :| "iterator.head" && {
					iterator.next(); iterator.head ?= array(until - 2)
				} :| "iterator.next().head"
	}
	new ReverseArrayIteratorProperty("next") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop = {
			val seq = Seq.fill(until - from)(iterator.next())
			(seq ?= ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse) &&
				iterator.next().throws[NoSuchElementException]
		}
	}
	new ArrayProperty("take") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ReverseArrayIterator.slice(array, from, until).take(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).reverse.take(n))
			}
	}
	new ArrayProperty("drop") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ReverseArrayIterator.slice(array, from, until).drop(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).reverse.drop(n))
			}
	}
	new ArrayProperty("slice") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, sliceFrom :Int, sliceUntil :Int) =>
				ReverseArrayIterator.slice(array, from, until).slice(sliceFrom, sliceUntil).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse.slice(sliceFrom, sliceUntil)
			}
	}
	new ArrayProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				val (first, second)    = ReverseArrayIterator.slice(array, from, until).splitAt(n)
				val (expect1, expect2) = ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse.splitAt(n)
				(first.toSeq ?= expect1) :| "_1" && (second.toSeq ?= expect2) :| "_2"
			}
	}
	new ArrayProperty("foldLeft") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val iter   = ReverseArrayIterator.slice(array, from, until)
				val string = iter.foldLeft("")((acc, x) => if (acc == "") x.toString + ")" else x.toString + ", " + acc)
				val result = if (string == "") "()" else "(" + string
				result ?= array.slice(from, until).mkString("(", ", ", ")")
			}
	}
	new ArrayProperty("reduceLeft") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val slice = array.slice(from, until)
				val iter  = ReverseArrayIterator.slice(array, from, until)
				if (slice.length == 0)
					iter.reduceLeft(Ordering[X].max).throws[UnsupportedOperationException]
				else
					iter.reduceLeft(Ordering[X].max) ?= slice.reduceLeft(Ordering[X].max)
			}
	}
	new ArrayProperty("reduceLeftOption") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val slice = array.slice(from, until)
				val iter  = ReverseArrayIterator.slice(array, from, until)
				iter.reduceLeftOption(Ordering[X].min) ?= slice.reduceLeftOption(Ordering[X].min)
			}
	}

}
