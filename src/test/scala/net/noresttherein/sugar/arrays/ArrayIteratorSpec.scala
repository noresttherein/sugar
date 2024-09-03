package net.noresttherein.sugar.arrays

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Prop, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter

import net.noresttherein.sugar.collections.ValIterator
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, Prettify, PropExtension}
import net.noresttherein.sugar.testing.scalacheck.typeClasses.arbitraryAny
import net.noresttherein.sugar.witness.NullValue




object  ArrayIteratorSpec extends ArrayTestingUtils("ArrayIterator") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(200)

	private def seq[T](it :Iterator[T]) :Seq[T] = it.toList

	//The public iterator type returned by ArrayIterator factory object.
	type TestedIterator[+T] = ValIterator.Buffered[T]

	property("ArrayIterator.apply") = forAll { (array :Array[Int], from :Int, length :Int) =>
		val iter = ArrayIterator(array, from, length)
		val expect = array.iterator.drop(from).take(length)
		iter sameElements expect lbl
			"ArrayIterator(" + array.contentsString + ", " + from + ", " + length + ") ?= " +
				ArrayIterator(array, from, length).mkString("Iterator(", ", ", ")")
	}

	private abstract class ArrayIteratorProperty(name :String) extends ArrayProperty(name) {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0  = from max 0 min array.length
				val until0 = until max from0 min array.length
				apply(array, from0, until0, ArrayIterator.slice(array, from, until))
			}

		def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop
	}

	new ArrayIteratorProperty("knownSize") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
			iterator.knownSize ?= (until - from)
	}
	new ArrayIteratorProperty("size") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
			iterator.size ?= (until - from)
	}
	new ArrayIteratorProperty("head") {
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop =
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
		override def apply[X :ClassTag](array :Array[X], from :Int, until :Int, iterator :TestedIterator[X]) :Prop = {
			val seq = Seq.fill(until - from)(iterator.next())
			(seq ?= ArraySeq.unsafeWrapArray(array.slice(from, until))) && iterator.next().throws[NoSuchElementException]
		}
	}
	new ArrayProperty("take") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				seq(ArrayIterator.slice(array, from, until).take(n)) ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).take(n))
			}
	}
	new ArrayProperty("drop") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				seq(ArrayIterator.slice(array, from, until).drop(n)) ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).drop(n))
			}
	}
	new ArrayProperty("slice") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, sliceFrom :Int, sliceUntil :Int) =>
				seq(ArrayIterator.slice(array, from, until).slice(sliceFrom, sliceUntil)) ?=
					ArraySeq.unsafeWrapArray(array.slice(from, until).slice(sliceFrom, sliceUntil))
			}
	}
	new ArrayProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) =>
				val (first, second) = ArrayIterator.slice(array, from, until).splitAt(n)
				val (expect1, expect2) = array.slice(from, until).splitAt(n)
				(seq(first) ?= ArraySeq.unsafeWrapArray(expect1)) :| "_1" &&
					(seq(second) ?= ArraySeq.unsafeWrapArray(expect2)) :| "_2"
			}
	}
	new ArrayProperty("foldLeft") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val iter = ArrayIterator.slice(array, from, until)
				//this is a weird way of putting it, but it avoids the pitfall of array(0) being an empty string
				val string = iter.foldLeft("")((acc, x) => if (acc == "") "(" + x else acc + ", " + x)
				val result = if (string == "") "()" else string + ")"
				result ?= array.slice(from, until).mkString("(", ", ", ")")
			}
	}
	new ArrayProperty("reduceLeft") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val slice = array.slice(from, until)
				val iter = ArrayIterator.slice(array, from, until)
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
				val iter = ArrayIterator.slice(array, from, until)
				iter.reduceLeftOption(Ordering[X].min) ?= slice.reduceLeftOption(Ordering[X].min)
			}
	}
	new ArrayProperty("copyToArray") {
		import net.noresttherein.sugar.testing.scalacheck.noShrinking
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val from0 = from max 0 min array.length
				val size = array.view.slice(from, until).length
				def iterator = ArrayIterator.slice(array, from, until)

				def property[Y >: X :ClassTag :Arbitrary] = forAll { (target :Array[Y], start :Int, len :Int) =>
					val bufferSize = target.length
					val start0     = start min bufferSize max 0
					val shouldCopy = len min size min bufferSize - start0 max 0
					val buffer     = target.clone()

					//The logic of whether to throw an IOOB in Scala is messed up.
					// In IterableOnceOps, it's start < 0 && size > 0 && start < start + math.min(len, bufferSize - start).
					// Note we can't subtract start from both sides because of underflows.
					// In ArrayOps, ArraySeq, Vector its len min srcLen min dstLen - start max 0
					// (which excludes min dstLen and is susceptible to overflow).
					(if (start < 0 && len > 0 && size > 0 && start < bufferSize)
						iterator.copyToArray(buffer, start, len).throws[IndexOutOfBoundsException] &&
							(ArraySeq.unsafeWrapArray(buffer) ?= ArraySeq.unsafeWrapArray(target))
					else {
						val copied = iterator.copyToArray(buffer, start, len)
						val end0   = start0 + copied
						if (array.toSeq.slice(from0, from0 + copied) != buffer.toSeq.slice(start, start + shouldCopy)) {
							Console.err.println("Expected: " + ArraySeq.unsafeWrapArray(buffer.slice(start, start + shouldCopy)))
							Console.err.println("Copied:   " + ArraySeq.unsafeWrapArray(array.slice(from0, from0 + copied)))
						}
						val prop = (copied ?= shouldCopy) &&
							((unsafeWrapArray(array).slice(from0, from0 + copied) :Seq[Y]) =?
								unsafeWrapArray(buffer).slice(start, start + copied) lbl "copied") &&
							(unsafeWrapArray(target).slice(0, start) =?
								unsafeWrapArray(buffer).slice(0, start) lbl "unmodified prefix") &&
							(unsafeWrapArray(target).slice(end0, bufferSize) =?
								unsafeWrapArray(buffer).slice(end0, bufferSize) lbl "unmodified suffix") lbl
								s"$iterator.copyToArray(${target.contentsString}, $start, $len)"
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
					}) lbl target.localClassName + "|" + target.length + "|"
				}
				property[X] && property[Any]
			}
	}
	new ArrayProperty("toIndexedSeq") {
		override def apply[X :ClassTag :Ordering :NullValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (first :Int, length :Int) =>
				val first0 = first max 0 min array.length
				val expect = ArraySeq.unsafeWrapArray(array.drop(first).take(length))
				("immutable" |: (IArrayLikeIterator(array.asInstanceOf[IArrayLike[X]], first0, length).toIndexedSeq
					?= expect)) &&
					("mutable" |: (ArrayIterator(array, first0, length).toIndexedSeq ?= expect))
			}
	}
}
