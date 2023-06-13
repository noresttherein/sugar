package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop, Shrink, Test}
import org.scalacheck.util.ConsoleReporter

import net.noresttherein.sugar.collections.extensions.ArrayExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, Prettify}
import net.noresttherein.sugar.witness.DefaultValue




object ArrayIteratorSpec extends ArrayTestingUtils("ArrayIterator") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))

	private abstract class ArrayIteratorProperty(name :String) extends ArrayProperty(name) {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0  = from max 0 min array.length
				val until0 = until max from0 min array.length
				apply(array, from0, until0, ArrayIterator(array, from, until))
			}

		def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ArrayIterator[X]) :Prop
	}

	new ArrayIteratorProperty("knownSize") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ArrayIterator[X]) :Prop =
			iterator.knownSize ?= (until - from)
	}
	new ArrayIteratorProperty("size") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ArrayIterator[X]) :Prop =
			iterator.size ?= (until - from)
	}
	new ArrayIteratorProperty("head") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ArrayIterator[X]) :Prop =
			if (until <= from)
				iterator.head.throws[NoSuchElementException]
			else if (until == from + 1)
				iterator.head ?= array(from)
			else
				(iterator.head ?= array(from)) :| "iterator.head" && {
					iterator.next(); iterator.head ?= array(from + 1)
				} :| "iterator.next().head"
	}
	new ArrayIteratorProperty("next") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :ArrayIterator[X]) :Prop = {
			val seq = Seq.fill(until - from)(iterator.next())
			(seq ?= ArraySeq.unsafeWrapArray(array.slice(from, until))) && iterator.next().throws[NoSuchElementException]
		}
	}
	new ArrayProperty("take") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ArrayIterator(array, from, until).take(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).take(n))
			}
	}
	new ArrayProperty("drop") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ArrayIterator(array, from, until).drop(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).drop(n))
			}
	}
	new ArrayProperty("slice") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, sliceFrom :Int, sliceUntil :Int) =>
				ArrayIterator(array, from, until).slice(sliceFrom, sliceUntil).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).slice(sliceFrom, sliceUntil))
			}
	}
	new ArrayProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				val (first, second) = ArrayIterator(array, from, until).splitAt(n)
				val (expect1, expect2) = array.slice(from, until).splitAt(n)
				(first.toSeq ?= ArraySeq.unsafeWrapArray(expect1)) :| "_1" &&
					(second.toSeq ?= ArraySeq.unsafeWrapArray(expect2)) :| "_2"
			}
	}
}
