package net.noresttherein.sugar.arrays

import scala.collection.{ArrayOps, BufferedIterator}
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter
import net.noresttherein.sugar.collections.ExpandedSliceFactory
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.extensions.{ThrowableExtension, classNameMethods}
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}
import net.noresttherein.sugar.testing.scalacheck.typeClasses.arbitraryAny




abstract class IndexedIteratorProps[S[_], I[X] <: BufferedIterator[X]]
                                   (override val name :String, protected val factory :ExpandedSliceFactory[S, I])
	extends Properties(name)
{
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000).withMaxSize(128)

//	implicit protected def buildableSource[X :ClassTag] :Buildable[X, S[X]] = new Buildable[X, S[X]] {
//		override def builder :Builder[X, S[X]] = sourceBuilder[X]
//	}
//	protected def sourceBuilder[X :ClassTag] :Builder[X, S[X]]

	protected def lengthOf[X](source :S[X]) :Int
	protected def seq[X](source :S[X]) :Seq[X]
	protected def seq[X](it :Iterator[X]) :Seq[X] = it.toList
	protected def slice[X](source :S[X], from :Int, until :Int) :Seq[X] = seq(source).slice(from, until)
	protected def iterator[X](source :S[X]) :Iterator[X]

	protected def expectSlice[X](source :S[X], from :Int, until :Int) :Seq[X] = slice(source, from, until)
	protected def expectApply[X](source :S[X], first :Int, length :Int) :Seq[X] = {
		val from = math.min(lengthOf(source), math.max(first, 0))
		val until = from + math.min(lengthOf(source) - from, math.max(length, 0))
		slice(source, from, until)
	}
	protected def expectFrom[X](source :S[X], first :Int) :Seq[X] = expectApply(source, first, Int.MaxValue)
	protected def contents[X](source :S[X]) :String = source.toString

	protected def mod(idx :Int, len :Int) :Int =
		if (len == 0) 0
		else if (idx < 0) (len + idx % len) % len
		else idx % len

	protected def mod(index :Long, length :Int) :Int =
		if (length == 0) 0
		else if (index >= 0) (index % length).toInt
		else (length + (index % length).toInt) % length

	protected def clip(idx :Long, length :Int) :Int =
		if (idx < 0) 0
		else if (idx > length) length
		else idx.toInt


	protected def forAllInputs(property :IteratorProperty) :Prop


	abstract class IteratorProperty(val propName :String) {
		def apply[X :ClassTag :Ordering :Arbitrary](source :S[X]) :Prop = forAll { (from :Int, until :Int) =>
			val expect = expectSlice(source, from, until)
			apply(expect, factory.slice(source, from, until)) lbl
				s"$name.slice(${contents(source)}, $from, $until) == " +
					factory.slice(source, from, until).mkString("Iterator(", ", ", ")")
		}
		def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			expect sameElements iterator

		property(propName) = forAllInputs(this)
	}


	new IteratorProperty(s"$name(source, first, length)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :S[X]) :Prop =
			forAll { (first :Int, length :Int) =>
				try {
					val expect = expectApply(source, first, length)
					apply(expect, factory(source, first, length)) lbl
						s"$name(${ contents(source) }, $first, $length) == " +
							factory(source, first, length).mkString("Iterator(", ", ", ")") + "\n!= " + expect
				} catch {
					case e :scala.Exception => e.printStackTrace(System.out); throw e
				}
			}
	}
	new IteratorProperty(s"$name.from") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :S[X]) :Prop = forAll { (first :Int) =>
			val expect = expectFrom(source, first)
			apply(expect, factory.from(source, first)) lbl
				s"$name.from(${contents(source)}, $first) == " +
					factory.from(source, first).mkString("Iterator(", ", ", ")") + "\n!= " + expect
		}
	}
	new IteratorProperty(s"$name.slice(source, from, until)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iter: => I[X]) =
			super.apply(expect, iter) lbl "!= " + expect
	}


	new IteratorProperty("knownSize") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			iterator.knownSize ?= expect.length
	}
	new IteratorProperty("size") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			iterator.size ?= expect.length
	}

	new IteratorProperty("head") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			if (expect.isEmpty)
				iterator.head.throws[NoSuchElementException]
			else if (expect.length == 1)
				iterator.head ?= expect.head
			else {
				val itr = iterator
				val heads = new ArrayBuffer[X](expect.length)
				while (itr.hasNext) {
					heads += itr.head
					itr.next()
				}
				(heads :collection.Seq[X]) ?= expect
			}
	}
	new IteratorProperty("next") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop = {
			val itr = iterator
			val seq = Seq.fill(expect.length)(itr.next())
			(seq ?= expect) && itr.next().throws[NoSuchElementException]
		}
	}

	new IteratorProperty("take") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			forAll { (from :Int, until :Int, n :Int) => seq(iterator.take(n)) ?= expect.take(n) }
	}
	new IteratorProperty("drop") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			forAll { (n :Int) => seq(iterator.drop(n)) ?= expect.drop(n) }
	}
	new IteratorProperty("slice") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			forAll { (sliceFrom :Int, sliceUntil :Int) =>
				seq(iterator.slice(sliceFrom, sliceUntil)) ?= expect.slice(sliceFrom, sliceUntil)
			}
	}
	new IteratorProperty("splitAt") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			forAll { (n :Int) =>
				val (first, second)    = iterator.splitAt(n)
				val (expect1, expect2) = expect.splitAt(n)
				try {
					(seq(first) ?= expect1) :| "_1" && (seq(second) ?= expect2) :| "_2"
				} catch { case e :scala.Exception => e.printStackTrace(System.err); throw e }
			}
	}

	new IteratorProperty("foldLeft") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop = {
			//this is a weird way of putting it, but it avoids the pitfall of array(0) being an empty string
			val string = iterator.foldLeft("")((acc, x) => if (acc == "") "(" + x else acc + ", " + x)
			val result = if (string == "") "()" else string + ")"
			result ?= expect.mkString("(", ", ", ")")
		}
	}
	new IteratorProperty("reduceLeft") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop =
			if (expect.length == 0)
				iterator.reduceLeft(Ordering[X].max).throws[UnsupportedOperationException]
			else
				iterator.reduceLeft(Ordering[X].max) ?= expect.reduceLeft(Ordering[X].max)
	}
	new IteratorProperty("reduceLeftOption") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop = {
			iterator.reduceLeftOption(Ordering[X].min) ?= expect.reduceLeftOption(Ordering[X].min)
		}
	}

	new IteratorProperty("copyToArray") {
		import net.noresttherein.sugar.testing.scalacheck.noShrinking
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator: => I[X]) :Prop = {

			def property[Y >: X :ClassTag :Arbitrary] = forAll { (target :Array[Y], start :Int, len :Int) =>
				val size       = expect.length
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
					val copied = try iterator.copyToArray(buffer, start, len) catch {
						case e :scala.Exception => throw e.addInfo(s"copyToArray(${errorString(buffer)}, $start, $len)")
					}
					val end0   = start0 + copied
//					if (expect != buffer.toSeq.slice(start, start + shouldCopy)) {
//						Console.err.println("Expected: " + expect.take(copied))
//						Console.err.println("Copied:   " + ArraySeq.unsafeWrapArray(buffer.slice(start0, start0 + copied)))
//					}
					val prop = (copied ?= shouldCopy) &&
						((expect.take(copied) :Seq[Y]) =?
							unsafeWrapArray(buffer).slice(start, start + copied) lbl "copied") &&
						(unsafeWrapArray(target).slice(0, start) =?
							unsafeWrapArray(buffer).slice(0, start) lbl "unmodified prefix") &&
						(unsafeWrapArray(target).slice(end0, bufferSize) =?
							unsafeWrapArray(buffer).slice(end0, bufferSize) lbl "unmodified suffix") lbl
						s"$iterator.copyToArray(${target.contentsString}, $start, $len)"
//					prop.map { res =>
//						if (res.failure) {
////							Console.err.println(s"CyclicArrayIterator.over(${array.contentsString}, $from, $until)")
//							Console.err.println(s".copyToArray(${target.contentsString}, $start, $len)")
//							Console.err.println(s"\tcopied $copied; should copy: $shouldCopy")
//							Console.err.println(s"\tresult: ${buffer.contentsString}")
//							Console.err.println(s"\twritten: ${buffer.slice(start0, start0 + copied).contentsString}")
//						}
//						res
//					}
					prop
				}) lbl target.localClassName + "|" + target.length + "|"
			}
			property[X] && property[Any]
		}
	}

	new IteratorProperty("toIndexedSeq") {
		override def apply[X :ClassTag :Ordering :Arbitrary](expect :Seq[X], iterator : => I[X]) :Prop =
			(iterator.toIndexedSeq :Seq[X]) ?= expect
	}
}



abstract class ArrayIteratorProps[I[X] <: BufferedIterator[X]](name :String, factory :ExpandedSliceFactory[Array, I])
	extends IndexedIteratorProps[Array, I](name, factory)
{
//	protected override def sourceBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder

	protected override def lengthOf[A](source :Array[A]) :Int = source.length
	protected override def seq[A](source :Array[A]) :Seq[A] = ArraySeq.unsafeWrapArray(source)
	protected override def iterator[A](source :Array[A]) :Iterator[A] = new ArrayOps(source).iterator
	protected override def contents[X](source :Array[X]) :String = source.contentsString

	protected override def forAllInputs(prop :IteratorProperty) :Prop =
		forAll { a :Array[Byte]    => prop(a.clone()) :| "Array[Byte]" lbl a.contentsString} &&
		forAll { a :Array[Short]   => prop(a.clone()) :| "Array[Short]" lbl a.contentsString } &&
		forAll { a :Array[Char]    => prop(a.clone()) :| "Array[Char]" lbl a.contentsString } &&
		forAll { a :Array[Int]     => prop(a.clone()) :| "Array[Int]" lbl a.contentsString } &&
		forAll { a :Array[Long]    => prop(a.clone()) :| "Array[Long]" lbl a.contentsString } &&
		forAll { a :Array[Float]   => prop(a.clone()) :| "Array[Float]" lbl a.contentsString } &&
		forAll { a :Array[Double]  => prop(a.clone()) :| "Array[Double]" lbl a.contentsString } &&
		forAll { a :Array[Boolean] => prop(a.clone()) :| "Array[Boolean]" lbl a.contentsString } &&
		forAll { a :Array[Unit]    => prop(Array.copyOf(a, a.length)) :| "Array[Unit]" lbl a.contentsString } &&
		forAll { a :Array[String]  => prop(a.clone()) :| "Array[String]" lbl a.contentsString }
}




object ArrayIteratorSpec extends ArrayIteratorProps[BufferedIterator]("ArrayIterator", ArrayIterator)
