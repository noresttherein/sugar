package net.noresttherein.sugar.collections

import scala.collection.immutable.{ArraySeq, SortedSet}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}
import org.scalacheck.Prop.{?=, AnyOperators, all, forAll, throws}
import org.scalacheck.util.{Buildable, ConsoleReporter}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}




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

	private def prop(prop :(StringSet, SortedSet[String]) => Prop) :Prop = {
		implicit val arbitrary :Arbitrary[Seq[String]] = Arbitrary(Gen.listOf(Gen.alphaNumStr))
		forAll { elems :Seq[String] =>
			val expect  = elems to SortedSet
			val subject = elems to StringSet
			prop(subject, expect) lbl expect.toString
		}
	}
	//		forAll { chars :List[List[Short]] => //use Short because non-printable Chars make it hard to debug
//			val chops  = chars.map(_.foldLeft(new StringBuilder)(_ += _.toChar).toString)
//			val string = chops.foldLeft(new StringBuilder)(_ ++= _).toString
//			prop(ChoppedString(chops), string) && forAll { (from :Int, until :Int) =>
//				prop(ChoppedString(chops).slice(from, until), substring(from, until)(string))
//			} lbl chops.toString
//		}

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
	property("at")                 = prop { (strings, expect) =>
		all((0 until expect.size).map(i => (strings.at(i) ?= expect.drop(i).head) :| "#" + i) :_*) &&
			strings.at(-2).throws[IndexOutOfBoundsException] &&
			strings.at(expect.size).throws[IndexOutOfBoundsException]
	}
	property("contains")           = prop { (strings, expect) => forAll { (string :String) =>
		strings.contains(string) ?= expect.contains(string)
	}}
	property("+")                  = prop { (strings, expect) => forAll { (string :String) =>
		(strings + string :Set[String]) ?= (expect + string)
	}}
	property("iterator")           = prop { (strings, expect) =>
		strings.iterator.to(ArrayBuffer) ?= expect.to(ArrayBuffer)
	}

	property("foreach")            = prop { (strings, expect) =>
		val res = new ArrayBuffer[String]
		strings.foreach(res += _)
		res ?= expect.to(ArrayBuffer)
	}
	property("foldLeft")           = prop { (strings, expect) =>
		strings.foldLeft("")(_ + _) ?= expect.foldLeft("")(_ + _)
	}
	property("foldRight")          = prop { (strings, expect) =>
		strings.foldRight("")(_ + _) ?= expect.foldRight("")(_ + _)
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

	property("union")            = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings | other, expect | other) }
	}
	property("intersect")            = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings & other, expect & other) }
	}
	property("diff")            = prop { (strings, expect) =>
		forAll { other :StringSet => props(strings &~ other, expect &~ other) }
	}

	property("to") = prop { (strings, expect) => (strings to ArraySeq) ?= (expect to ArraySeq) }



	def props(result :Set[String], reference :Set[String]) :Prop =
		props(reference.toString)(result)(reference)

	private def debug(name :String)(prop :Prop) = prop.map { res =>
//		if (res.failure) {
//			Console.err.println(name + " " + res)
//			Console.err.println(res.labels.mkString("labels: ", ", ", ""))
//		}
		res
	}
	def props(name :String)(result :Set[String])(implicit reference :Set[String]) =
		((result :Set[String]) ?= reference) && (name |: all(
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

			"take"               |: debug("take")(forAll { i :Int => result.take(i) ?= reference.take(i) }),
			"drop"               |: debug("drop")(forAll { i :Int => result.drop(i) ?= reference.drop(i) }),
			"takeRight"          |: debug("takeRight")(forAll { i :Int =>
			                        	result.takeRight(i) ?= reference.takeRight(i)
			                     }),
			"dropRight"          |: debug("dropRight")(forAll { i :Int =>
			                        	result.dropRight(i) ?= reference.dropRight(i)
			                     }),
			"slice"              |: debug("slice")(
			                       forAll { (i :Int, j :Int) => result.slice(i, j) ?= reference.slice(i, j)
		                         }),
			"splitAt"            |: debug("splitAt")(forAll { i :Int => result.splitAt(i) ?= reference.splitAt(i) }),
			"takeWhile"          |: debug("takeWhile")(
			                        result.takeWhile(startsWithLower) ?= reference.takeWhile(startsWithLower)
			                     ),
			"dropWhile"          |: debug("dropWhile")(
			                        result.dropWhile(startsWithLower) ?= reference.dropWhile(startsWithLower)
			                     ),
			"span"               |: debug("span")(result.span(startsWithLower) ?= reference.span(startsWithLower)),
		))

	private def startsWithLower(str :String) = str.length > 0 && str.charAt(0).isLower
}
