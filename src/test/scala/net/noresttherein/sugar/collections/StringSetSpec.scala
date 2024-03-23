package net.noresttherein.sugar.collections

import scala.collection.immutable.{ArraySeq, SortedSet}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.collections.ElementIndex.{Absent, Present}
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}
import org.scalacheck.Prop.{?=, AnyOperators, all, forAll, throws}
import org.scalacheck.util.{Buildable, ConsoleReporter}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}




//todo: StringMapSpec
object StringSetSpec extends Properties("StringSet") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(200).withMaxSize(256)

	val parameters = overrideParameters(Test.Parameters.default)
//	implicit val buildable :Buildable[String, StringSet] = new Buildable[String, StringSet] {
//		override def builder :Builder[String, StringSet] = StringSet.newBuilder
//	}

	import net.noresttherein.sugar.testing.scalacheck.typeClasses.shrinkAlphaNumString

	implicit val arbitrary :Arbitrary[StringSet] = Arbitrary {
		Gen.listOf(Gen.alphaNumStr).map(StringSet.fromSpecific)
	}

	//We use set.size as a label everywhere, because StringSet("") and StringSet() look exactly the same.
	private def prop(prop :(StringSet, SortedSet[String]) => Prop) :Prop = {
		implicit val arbitrary :Arbitrary[Seq[String]] = Arbitrary(Gen.listOf(Gen.alphaNumStr))
		forAll { elems :Seq[String] =>
			val expect  = elems to SortedSet
			val subject = elems to StringSet
			prop(subject, expect) lbl "Input:   " + expect + " (" + expect.size + " elements)" lbl
				"Testing: " + subject + " (" + subject.size + " elements)"
		}
	}

	//todo; test patch methods
	property("size")               = prop { (strings, expect) => strings.size ?= expect.size }
	property("knownSize")          = prop { (strings, expect) => strings.knownSize ?= expect.size }
	property("head")               = prop { (strings, expect) =>
		if (expect.size == 0) throws(classOf[NoSuchElementException])(strings.head)
		else strings.head ?= expect.head
	}
	property("last")               = prop { (strings, expect) =>
		if (expect.size == 0) throws(classOf[NoSuchElementException])(strings.last)
		else strings.last ?= expect.last
	}
	property("key")                 = prop { (strings, expect) =>
		all((0 until expect.size).map(i => (strings.key(i) ?= expect.drop(i).head) :| "#" + i) :_*) &&
			strings.key(-2).throws[IndexOutOfBoundsException] &&
			strings.key(expect.size).throws[IndexOutOfBoundsException]
	}
	property("indexOf")            = prop { (strings, expect) =>
		val indexed = expect.toSeq.zipWithIndex
		all(indexed.map {
			case (key, idx) => Present(idx) =? strings.indexOf(key) lbl "indexOf(" + key + ")"
		} :_*) && forAll { key :String =>
			if (expect(key))
				Present(indexed.indexWhere(_._1 == key)) =? strings.indexOf(key)
			else
				Absent(indexed.iterator.takeWhile(_._1 < key).size) =? strings.indexOf(key)
		}
	}
	property("contains")           = prop { (strings, expect) => forAll { (string :String) =>
		strings.contains(string) ?= expect.contains(string)
	}}
	property("+")                  = prop { (strings, expect) => forAll { (string :String) =>
		(strings + string :Set[String]) ?= (expect + string)
	}}
	property("iterator")           = prop { (strings, expect) =>
		val tested = strings.iterator.to(ArrayBuffer)
		expect.to(ArrayBuffer) =? tested lbl "subject.iterator.size: " + tested.size
	}
	//This property essentially tests StringSet.slice working on large sets, without taxing scalacheck
	property("iterator.slice") = prop { (strings, expect) =>
		forAll {
			(from :Int, until :Int) => expect.slice(from, until) =? strings.iterator.slice(from, until).to(StringSet)
		}
	}

	property("foreach")            = prop { (strings, expect) =>
		val res = new ArrayBuffer[String]
		strings.foreach(res += _)
		expect.to(ArrayBuffer) =? res lbl "result.size: " + res.size
	}
	property("foldLeft")           = prop { (strings, expect) =>
		strings.foldLeft("'")(_ + _) + "'" ?= expect.foldLeft("'")(_ + _) + "'"
	}
	property("foldRight")          = prop { (strings, expect) =>
		strings.foldRight("'")(_ + _) + "'" ?= expect.foldRight("'")(_ + _) + "'"
	}

	property("take")               = prop { (strings, expect) =>
		forAll { n :Int => props(strings.take(n), expect.take(n)) }
	}
	property("drop")               = prop { (strings, expect) =>
		forAll { n :Int => props(strings.drop(n), expect.drop(n)) }
	}
	property("takeRight")          = prop { (strings, expect) =>
		forAll { n :Int => props(strings.takeRight(n), expect.takeRight(n)) }
	}
	property("dropRight")          = prop { (strings, expect) =>
		forAll { n :Int => props(strings.dropRight(n), expect.dropRight(n)) }
	}
	property("slice")              = prop { (strings, expect) =>
		forAll { (i :Int, j :Int) => props(strings.slice(i, j), expect.slice(i, j)) }
	}
	property("splitAt")            = prop { (strings, expect) => forAll { n :Int =>
		val (left, right) = strings.splitAt(n)
		val (leftExpect, rightExpect) = expect.splitAt(n)
		props("_1")(left)(leftExpect) && props("_2")(right)(rightExpect)
	}}
	property("takeWhile")          = prop { (strings, expect) =>
		props(strings.takeWhile { str => str.length == 0 || str.head < 'n' },
			expect.takeWhile(str => str.length == 0 || str.head < 'n'))
	}
	property("dropWhile")          = prop { (strings, expect) =>
		props(strings.dropWhile(str => str.length == 0 || str.head < 'n'),
			expect.dropWhile(str => str.length == 0 || str.head < 'n'))
	}
	property("span")               = prop { (strings, expect) =>
		val (left, right) = strings.span(str => str.length == 0 || str.head < 'n')
		val (leftExpect, rightExpect) = expect.span(str => str.length == 0 || str.head < 'n')
		props("_1")(left)(leftExpect) && props("_2")(right)(rightExpect)
	}
	property("map")                = prop { (strings, expect) =>
		(strings.map(_.toLowerCase) :Set[String]) ?= expect.map(_.toLowerCase)
	}
	property("flatMap")            = prop { (strings, expect) =>
		(strings.flatMap(str => ArraySeq.unsafeWrapArray(str.split("[0-9]"))) :Set[String]) ?=
			expect.flatMap(str => ArraySeq.unsafeWrapArray(str.split("[0-9]")))
	}

	property("union")              = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings | other, expect | other) }
	}
	property("intersect")          = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings & other, expect & other) }
	}
	property("diff")               = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings &~ other, expect &~ other) }
	}

	property("to")                 = prop { (strings, expect) =>
		(strings to ArraySeq) ?= (expect to ArraySeq)
	}

	property("range")              = prop { (strings, expect) =>
		forAll { (from :String, until :String) =>
			expect.range(from, until) =? strings.range(from, until)
		}
	}
	property("rangeImpl")          = prop { (strings, expect) =>
		forAll { (from :Option[String], until :Option[String]) =>
			expect.rangeImpl(from, until) =? strings.rangeImpl(from, until)
		}
	}
	property("rangeFrom")          = prop { (strings, expect) =>
		forAll { (from :String) =>
			expect.rangeFrom(from) =? strings.rangeFrom(from)
		}
	}
	property("rangeUntil")         = prop { (strings, expect) =>
		forAll { (until :String) =>
			expect.rangeUntil(until) =? strings.rangeUntil(until)
		}
	}
	property("iteratorFrom")       = prop { (strings, expect) =>
		forAll { key :String => expect.iteratorFrom(key).toSeq =? strings.iteratorFrom(key).toSeq }
	}
	property("reverseIterator")    = prop { (strings, expect) =>
		expect.toSeq.reverse =? strings.reverseIterator.toSeq
	}

	private def debug(name :String)(prop :Prop) = prop.map { res =>
//		if (res.failure) {
//			Console.err.println(name + " " + res)
//			Console.err.println(res.labels.mkString("labels: ", ", ", ""))
//		}
		res
	}

	def props(result :Set[String], reference :Set[String]) :Prop =
		props("Expected: " + reference + " (" + reference.size + " elements)")(result)(reference)

	def props(name : =>String)(result :Set[String])(implicit reference :Set[String]) =
		((result :Set[String]) ?= reference) && (name lbl_: all(
			"size"               |: debug("size")(result.size ?= reference.size),
			"knownSize"          |: debug("knownSize")(result.knownSize ?= reference.size),
			"head"               |: debug("head")(
			                          if (reference.size == 0) Prop(throws(classOf[NoSuchElementException])(result.head))
			                          else result.head ?= reference.head
			                     ),
			"last"               |: debug("last")(
			                          if (reference.size == 0) Prop(throws(classOf[NoSuchElementException])(result.last))
			                          else result.last ?= reference.last
			                     ),
			"contains"           |: debug("contains")(
			                       all(reference.toSeq.map(s => (result(s) ?= reference(s)) :| "contains(" + s + ")") :_*)
			                     ),
			"iterator"           |: debug("iterator")(
			                        (ArrayBuffer.empty[String] ++= result.iterator) ?=
				                        reference.to(ArrayBuffer)
			                     ),
			"foreach"            |: debug("foreach"){
			                       val res = ArrayBuffer.empty[String]
			                       result.foreach(res += _)
			                       res ?= reference.to(ArrayBuffer)
			                     },

			"take"               |: debug("take")(forAll { i :Int =>
			                            reference.take(i) =? result.take(i) lbl
				                            reference.take(i).size.toString + " vs. " + result.take(i).size
			                     }),
			"drop"               |: debug("drop")(forAll { i :Int =>
			                            reference.drop(i) =? result.drop(i) lbl
				                            reference.drop(i).size.toString + " vs. " + result.drop(i).size
			                      }),
			"takeRight"          |: debug("takeRight")(forAll { i :Int =>
			                        	reference.takeRight(i) =? result.takeRight(i) lbl
					                        reference.takeRight(i).size.toString + " vs. " + result.takeRight(i).size
			                     }),
			"dropRight"          |: debug("dropRight")(forAll { i :Int =>
			                        	reference.dropRight(i) =? result.dropRight(i) lbl
					                        reference.dropRight(i).size.toString + " vs. " + result.dropRight(i).size
			                     }),
			"slice"              |: debug("slice")(
			                       forAll { (i :Int, j :Int) =>
				                       reference.slice(i, j) =? result.slice(i, j) lbl
					                       reference.slice(i, j).size.toString + " vs. " + result.slice(i, j).size
		                         }),
			"splitAt"            |: debug("splitAt")(forAll { i :Int => result.splitAt(i) ?= reference.splitAt(i) }),
			"takeWhile"          |: debug("takeWhile")(
			                        result.takeWhile(startsWithLower) ?= reference.takeWhile(startsWithLower)
			                     ),
			"dropWhile"          |: debug("dropWhile")(
			                        result.dropWhile(startsWithLower) ?= reference.dropWhile(startsWithLower)
			                     ),
			"span"               |: debug("span")(result.span(startsWithLower) ?= reference.span(startsWithLower)),
		)) lbl "Result:   " + result + " (" + result.size + " elements)"

	private def startsWithLower(str :String) = str.length > 0 && str.charAt(0).isLower
}
