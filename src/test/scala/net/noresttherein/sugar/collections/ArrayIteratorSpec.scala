package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop, Shrink, Test}
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.collections.extensions.ArrayExtension
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, Prettify}
import net.noresttherein.sugar.witness.DefaultValue




object ArrayIteratorSpec extends ArrayTestingUtils("ArrayIterator") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))

	property("ArrayIterator.apply") = forAll { (array :Array[Int], from :Int, length :Int) =>
		if (from < 0 || from > array.length)
			ArrayIterator(array, from, length).throws[IndexOutOfBoundsException]
		else {
			val iter = ArrayIterator(array, from, length)
			val expect = array.iterator.drop(from).take(length)
			iter sameElements expect lbl
				"ArrayIterator(" + array.contentsString + ", " + from + ", " + length + ") ?= " +
					ArrayIterator(array, from, length).mkString("Iterator(", ", ", ")")
		}
	}

	private abstract class ArrayIteratorProperty(name :String) extends ArrayProperty(name) {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0  = from max 0 min array.length
				val until0 = until max from0 min array.length
				apply(array, from0, until0, ArrayIterator.over(array, from, until))
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
				ArrayIterator.over(array, from, until).take(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).take(n))
			}
	}
	new ArrayProperty("drop") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				ArrayIterator.over(array, from, until).drop(n).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).drop(n))
			}
	}
	new ArrayProperty("slice") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, sliceFrom :Int, sliceUntil :Int) =>
				ArrayIterator.over(array, from, until).slice(sliceFrom, sliceUntil).toSeq ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).slice(sliceFrom, sliceUntil))
			}
	}
	new ArrayProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				val (first, second) = ArrayIterator.over(array, from, until).splitAt(n)
				val (expect1, expect2) = array.slice(from, until).splitAt(n)
				(first.toSeq ?= ArraySeq.unsafeWrapArray(expect1)) :| "_1" &&
					(second.toSeq ?= ArraySeq.unsafeWrapArray(expect2)) :| "_2"
			}
	}
	new ArrayProperty("copyToArray") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0 = from max 0 min array.length
				val size = array.view.slice(from, until).length
				def iterator = ArrayIterator.over(array, from, until)

				def property[Y >: X :ClassTag :Arbitrary] = forAll { (target :Array[Y], start :Int, len :Int) =>
					val bufferSize = target.length
					val start0     = start min bufferSize max 0
					val shouldCopy = len min size min bufferSize - start0 max 0
					val buffer     = target.clone()
					if (start < 0 && shouldCopy > 0)
						iterator.copyToArray(buffer, start, len).throws[IndexOutOfBoundsException] &&
							(ArraySeq.unsafeWrapArray(buffer) ?= ArraySeq.unsafeWrapArray(target))
					else {
						val copied = iterator.copyToArray(buffer, start, len)
						val end0   = start0 + copied
						(copied ?= shouldCopy) &&
							((unsafeWrapArray(array).slice(from0, from0 + copied) :Seq[Y]) =?
								unsafeWrapArray(buffer).slice(start, start + copied) lbl "copied") &&
							(unsafeWrapArray(target).slice(0, start) =?
								unsafeWrapArray(buffer).slice(0, start) lbl "unmodified prefix") &&
							(unsafeWrapArray(target).slice(end0, bufferSize) =?
								unsafeWrapArray(buffer).slice(end0, bufferSize) lbl "unmodified suffix") lbl
								s"$iterator.copyToArray(${target.contentsString}, $start, $len)"
/*
						prop.map { res =>
							if (res.failure) {
								Console.err.println(s"ArrayIterator.over(${array.contentsString}, $from, $until)")
								Console.err.println(s".copyToArray(${target.contentsString}, $start, $len)")
								Console.err.println(s"\tcopied $copied; should copy: $shouldCopy")
								Console.err.println(s"\tresult: ${buffer.contentsString}")
								Console.err.println(s"\twritten: ${buffer.slice(start0, start0 + copied).contentsString}")
							}
							res
						}
*/
					}
				}
				property[X] && property[Any]
			}
	}
}
