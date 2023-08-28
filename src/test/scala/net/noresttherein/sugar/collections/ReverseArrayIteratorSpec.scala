package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.math.Ordering
import scala.reflect.ClassTag

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop, Shrink, Test}
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.collections.extensions.ArrayExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, Prettify, PropExtension}
import net.noresttherein.sugar.witness.DefaultValue



//todo: move it to the file with ArrayIteratorSpec
object ReverseArrayIteratorSpec extends ArrayTestingUtils("ReverseArrayIterator") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))


	property("ReverseArrayIterator.apply") = forAll { (array :Array[Int], start :Int, length :Int) =>
		if (start < 0 || start >= array.length)
			ReverseArrayIterator(array, start, length).throws[IndexOutOfBoundsException]
		else {
			val iter = ReverseArrayIterator(array, start, length)
			val expect = array.take(math.min(start, Int.MinValue - 1) + 1).reverseIterator.take(length)
			iter sameElements expect lbl
				"ReverseArrayIterator(" + array.contentsString + ", " + start + ", " + length + ") ==" +
					ReverseArrayIterator(array, start, length).mkString("Iterator(", ", ", ")")
		}
	}

	private abstract class ReverseArrayIteratorProperty(name :String) extends ArrayProperty(name) {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0  = from max 0 min array.length
				val until0 = until max from0 min array.length
				apply(array, from0, until0, ReverseArrayIterator.over(array, from, until))
			}

		def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ReverseArrayIterator[X]) :Prop
	}

	new ReverseArrayIteratorProperty("knownSize") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ReverseArrayIterator[X]) :Prop =
			iterator.knownSize ?= (until - from)
	}
	new ReverseArrayIteratorProperty("size") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ReverseArrayIterator[X]) :Prop =
			iterator.size ?= (until - from)
	}
	new ReverseArrayIteratorProperty("head") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ReverseArrayIterator[X]) :Prop =
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
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ReverseArrayIterator[X]) :Prop = {
			val seq = Seq.fill(until - from)(iterator.next())
			(seq ?= ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse) &&
				iterator.next().throws[NoSuchElementException]
		}
	}
	new ArrayProperty("take") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ReverseArrayIterator.over(array, from, until).take(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).reverse.take(n))
			}
	}
	new ArrayProperty("drop") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ReverseArrayIterator.over(array, from, until).drop(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).reverse.drop(n))
			}
	}
	new ArrayProperty("slice") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, sliceFrom :Int, sliceUntil :Int) =>
				ReverseArrayIterator.over(array, from, until).slice(sliceFrom, sliceUntil).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse.slice(sliceFrom, sliceUntil)
			}
	}
	new ArrayProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				val (first, second)    = ReverseArrayIterator.over(array, from, until).splitAt(n)
				val (expect1, expect2) = ArraySeq.unsafeWrapArray(array.slice(from, until)).reverse.splitAt(n)
				(first.toSeq ?= expect1) :| "_1" && (second.toSeq ?= expect2) :| "_2"
			}
	}

}
