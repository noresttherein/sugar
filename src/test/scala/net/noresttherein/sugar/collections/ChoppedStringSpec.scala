package net.noresttherein.sugar.collections

import scala.collection.immutable.{ArraySeq, WrappedString}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.jdk.CollectionConverters.IteratorHasAsScala

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{Buildable, ConsoleReporter, Pretty}




object ChoppedStringSpec extends Properties("ChoppedString") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(200).withMaxSize(256)

	val parameters = overrideParameters(Test.Parameters.default)
	implicit val buildable :Buildable[Char, ChoppedString] = new Buildable[Char, ChoppedString] {
		override def builder :Builder[Char, ChoppedString] = ChoppedString.newBuilder
	}
//	implicit val choppedStringGenerator :Arbitrary[ChoppedString]       = Arbitrary(
//		for {
//			len <- Gen.choose(0, parameters.maxSize)
//			seq <- Gen.buildableOfN[ChoppedString, Char](len, Gen.alphaChar)
//		} yield seq
//	)
//		Arbitrary(Arbitrary.arbitrary[Seq[Char]].map(_ to ChoppedString))


	sealed abstract class ChoppedStringTemplate {
		def apply() :ChoppedString
		def pretty :String
	}
	case class Append(prefix :ChoppedStringTemplate, string :String) extends ChoppedStringTemplate {
		override def apply() :ChoppedString = prefix() :++ string
		override def pretty :String = "Append(" + prefix.pretty + ", \"" + string + "\")"
		override def toString :String = prefix.toString + string
	}
	case class Prepend(prefix :String, suffix :ChoppedStringTemplate) extends ChoppedStringTemplate {
		override def apply() :ChoppedString = prefix ++: suffix()
		override def pretty :String = "Prepend(\"" + prefix + "\", " + suffix.pretty + ")"
		override def toString :String = prefix + suffix
	}
	case class Cat(prefix :ChoppedStringTemplate, suffix :ChoppedStringTemplate) extends ChoppedStringTemplate {
		override def apply() :ChoppedString = prefix() :++ suffix()
		override def pretty :String = "Cat(" + prefix.pretty + ", " + suffix.pretty + ")"
		override def toString :String = prefix.toString + suffix
	}
	case class Slice(string :String, from :Int, until :Int) extends ChoppedStringTemplate {
		override def apply() :ChoppedString = string.slice(from, until)
		override def pretty :String = "Slice(\"" + string + "\", " + from + ", " + until + ")"
		override def toString :String = string.substring(from, until)
	}
	case object Empty extends ChoppedStringTemplate {
		override def apply() :ChoppedString = ChoppedString.empty
		override def pretty = "Empty"
		override def toString = ""
	}

	implicit def prettyChoppedStringTemplate(template :ChoppedStringTemplate) :Pretty = Pretty { _ => template.pretty }

	implicit val arbitraryChoppedStringTemplate :Arbitrary[ChoppedStringTemplate] = Arbitrary {
		import arbitraryChoppedStringTemplate.arbitrary
		Gen.oneOf(
			Gen.const(Empty),
			for { string <- Gen.alphaNumStr; until <- Gen.choose(0, string.length); from <- Gen.choose(0, until) }
				yield Slice(string, from, until),
			Gen.lzy(for { prefix <- arbitrary; suffix <- Gen.alphaNumStr } yield Append(prefix, suffix)),
			Gen.lzy(for { prefix <- Gen.alphaNumStr; suffix <- arbitrary } yield Prepend(prefix, suffix)),
			Gen.lzy(for { prefix <- arbitrary; suffix <- arbitrary } yield Cat(prefix, suffix))
		)
	}

	implicit val choppedStringShrink :Shrink[ChoppedString] = Shrink { string :ChoppedString =>
		implicitly[Shrink[Seq[Char]]].shrink(string).map(_ to ChoppedString)
	}


	val you = ChoppedString("you")
	val boo = ChoppedString("boo")
	val chunks = Seq(
		("ChoppedString.empty", ChoppedString.empty, ""),
		("ChoppedString.fromString(\"boo\")", ChoppedString("boo"), "boo"),
	)

	implicit def PropertiesToProp(properties :Properties) :Prop =
		all(properties.properties.map { case (name, prop) => prop :| name } to Seq :_*)

//	private def include(name :String)(props :Properties*) :Unit =
//		include((new Properties(name) /: props) { (acc, prop) => acc include prop; acc })



	include(ChoppedStringCompanionProps)

	private def prop(prop :(ChoppedString, String) => Prop) :Prop =
		forAll { template :ChoppedStringTemplate =>
			val string = template.toString
			val chops  = template()
			prop(chops, string) lbl "\"" + string + "\""
		}
//		forAll { chars :List[List[Short]] => //use Short because non-printable Chars make it hard to debug
//			val chops  = chars.map(_.foldLeft(new StringBuilder)(_ += _.toChar).toString)
//			val string = chops.foldLeft(new StringBuilder)(_ ++= _).toString
//			prop(ChoppedString(chops), string) && forAll { (from :Int, until :Int) =>
//				prop(ChoppedString(chops).slice(from, until), substring(from, until)(string))
//			} lbl chops.toString
//		}

	//todo; test patch methods
	property("length")             = prop { (chops, expect) => chops.length ?= expect.length }
	property("knownSize")          = prop { (chops, expect) => chops.knownSize ?= expect.length }
	property("head")               = prop { (chops, expect) =>
	                                 	if (expect.length == 0) throws(classOf[NoSuchElementException])(chops.head)
	                                 	else chops.head ?= expect(0)
	                                 }
	property("last")               = prop { (chops, expect) =>
	                                 	if (expect.length == 0) throws(classOf[NoSuchElementException])(chops.last)
	                                 	else chops.last ?= expect(expect.length - 1)
	                                 }
	property("apply")              = prop { (chops, expect) =>
	                                    all((0 until expect.length).map(i => (chops(i) ?= expect(i)) :| "#" + i) :_*)
	                                 }
	property("updated")            = prop { (chops, expect) => forAll { (i :Int, char :Char) =>
	                                    if (i < 0 || i >= expect.length)
		                                    chops.updated(i, char).throws[IndexOutOfBoundsException]
	                                    else
		                                    (chops.updated(i, char) :Seq[Char]) ?= (expect :Seq[Char]).updated(i, char)
	                                 }}
	property("iterator")           = prop { (chops, expect) => chops.iterator.to(ArrayBuffer) ?= expect.to(ArrayBuffer) }
	property("intIterator")        = prop { (chops, expect) => chops.intIterator.asScala.to(Seq) ?= expect.map(_.toInt) }
	property("javaIterator")       = prop { (chops, expect) => chops.jiterator.asScala.toSeq ?= expect.map(_.toInt) }

	property("isWhitespace")       = prop { (chops, expect) => chops.isWhitespace ?= expect.forall(_.isWhitespace) }

	property("foreach")            = prop { (chops, expect) =>
	                                    var res = 0; chops.foreach(res += _)
	                                    res ?= (0 /: expect)(_ + _)
	                                 }
	property("foldLeft")           = prop { (chops, expect) => chops.foldLeft(0)(_ + _) ?= expect.foldLeft(0)(_ + _) }
	property("foldRight")          = prop { (chops, expect) => chops.foldRight(0)(_ + _) ?= expect.foldRight(0)(_ + _) }

	property("take")               = prop { (chops, expect) => forAll { n :Int => props(chops.take(n), expect.take(n)) } }
	property("drop")               = prop { (chops, expect) => forAll { n :Int => props(chops.drop(n), expect.drop(n)) } }
	property("takeRight")          = prop { (chops, expect) =>
	                                 	forAll { n :Int => props(chops.takeRight(n), expect.takeRight(n)) }
	                                 }
	property("dropRight")          = prop { (chops, expect) =>
	                                 	forAll { n :Int => props(chops.dropRight(n), expect.dropRight(n)) }
	                                 }
	property("slice")              = prop { (chops, expect) =>
	                                 	forAll { (i :Int, j :Int) => props(chops.slice(i, j), expect.slice(i, j)) }
	                                 }
	property("splitAt")            = prop { (chops, expect) => forAll { n :Int =>
	                                    val (leftChops, rightChops) = chops.splitAt(n)
	                                    val (leftExpect, rightExpect) = expect.splitAt(n)
	                                    props("_1")(leftChops)(leftExpect) && props("_2")(rightChops)(rightExpect)
	                                 }}
	property("takeWhile")          = prop { (chops, expect) =>
	                                 	props(chops.takeWhile(_ % 9 < 7), expect.takeWhile(_ % 9 < 7))
	                                 }
	property("dropWhile")          = prop { (chops, expect) =>
	                                 	props(chops.dropWhile(_ % 9 < 7), expect.dropWhile(_ % 9 < 7))
	                                 }
	property("span")               = prop { (chops, expect) =>
	                                    val (leftChops, rightChops) = chops.span(_ % 9 < 7)
	                                    val (leftExpect, rightExpect) = expect.span(_ % 9 < 7)
	                                    props("_1")(leftChops)(leftExpect) &&
		                                    props("_2")(rightChops)(rightExpect)
	                                 }
	property("subSequence")        = prop { (chops, expect) =>
	                                 	forAll { (i :Int, j :Int) =>
		                                    if (i < 0 || j < 0 || i > j || j > expect.length)
			                                    Prop(throws(classOf[IndexOutOfBoundsException])(chops.subSequence(i, j)))
		                                    else
		                                        chops.subSequence(i, j).toString ?= expect.substring(i, j)
	                                    }
	                                 }
	property("map")                = prop { (chops, expect) =>
	                                    chops.map(_.toLower).toString ?= expect.map(_.toLower)
	                                 }
	property("flatMap")            = prop { (chops, expect) =>
	                                    chops.flatMap(ChoppedString(_) :++ ", ").toString ?=
		                                    expect.flatMap(_.toString + ", ")
	                                 }
	property("+(Char)")            = prop { (chops, expect) =>
	                                 	forAll { char :Char => props(chops + char, expect + char) }
	                                 }
	property("+:(Char)")           = prop { (chops, expect) =>
	                                  forAll { char :Char => props(char +: chops, char.toString + expect) }
	                                 }
	property(":++(String)")        = prop { (chops, expect) => forAll { s :String => props(chops :++ s, expect + s) } }
	property("++:(String)")        = prop { (chops, expect) => forAll { s :String => props(s ++: chops, s + expect) } }
	property(":++(ChoppedString)") = prop { (chops, expect) => forAll { s :String => props(s ++: chops, s + expect) } }
	property("++:(ChoppedString)") = prop { (chops, expect) => forAll { s :String => props(s ++: chops, s + expect) } }
//	include(props("+")(boo + 'I',"booI"))
//	include(props("+:")('I' +: boo, "Iboo"))
//	include(props("++(String)")(you ++ "boo", "youboo"))
//	include(props("++:(String)")("you" ++: boo, "youboo"))
//	include(props("++(ChoppedString)")(you ++ boo, "youboo"))
//	include(props("++:(ChoppedString)")(you ++: boo, "youboo"))

	property("to") =
		"you and boo and I".iterator.to(ChoppedString).to(List) ?= "you and boo and I".to(List)



	private def substring(from :Int, until :Int = Int.MaxValue)(implicit string :String) :String = {
		val i = from max 0 min string.length
		string.substring(i, until min string.length max i)
	}

	def props(result :ChoppedString, reference :String) :Prop =
		props("\"" + reference + "\"")(result)(reference).map { res =>
			if (res.failure) {
				Console.err.println(res)
				res.status match {
					case Prop.Exception(e) => e.printStackTrace(System.err)
					case _ =>
				}
				Console.err.println("Expected: " + reference + ";\ngot:     " + result + ": " + result.localClassName)
				if (result.toString == reference)
					Console.err.println("But they are equal???")
				Console.err.println(
					res.args.map(arg => "  " + arg.label + "=" + arg.prettyArg + " (orig " + arg.prettyOrigArg + ")")
					   .mkString("args:\n", "\n", "\n")
				)
				Console.err.println(res.labels.map("  " + _).mkString("labels:\n", "\n", "\n"))
			}
			res
		}

	private def debug(name :String)(prop :Prop) = prop.map { res =>
		if (res.failure) {
			Console.err.println(name + " " + res)
			Console.err.println(res.labels.mkString("labels: ", ", ", ""))
		}
		res
	}
	def props(name :String)(result :ChoppedString)(implicit reference :String) =
		((result :Seq[Char]) ?= reference.toSeq) && (name |: all(
			"length"             |: debug("length")(result.length ?= reference.length),
			"knownSize"          |: debug("knownSize")(result.knownSize ?= reference.knownSize),
			"head"               |: debug("head")(if (reference.length == 0) Prop(throws(classOf[NoSuchElementException])(result.head))
			                        else result.head ?= reference(0)),
			"last"               |: debug("last")(if (reference.length == 0) Prop(throws(classOf[NoSuchElementException])(result.last))
			                        else result.last ?= reference(reference.length - 1)),
			"apply"              |: debug("apply")(all((0 until reference.length).map(i => (result(i) ?= reference(i)) :| "#" + i):_*)),
			"iterator"           |: debug("iterator")((ArrayBuffer.empty[Char] ++= result.iterator) ?= reference.to(ArrayBuffer)),
			"stepper"            |: debug("stepper")(result.stepper.javaIterator.asScala.to(ArrayBuffer) ?=
			                            reference.map(Integer.valueOf(_)).to(ArrayBuffer)),
			"intIterator"        |: debug("intIterator")(result.intIterator.asScala.to(IndexedSeq) ?= reference.map(_.toInt)),
			"javaIterator"       |: debug("javaIterator")(result.jiterator.asScala.toSeq ?= reference.map(_.toInt)),

			"isWhitespace"       |: debug("isWhitespace")(result.isWhitespace ?= reference.forall(_.isWhitespace)),

			"foreach"            |: debug("foreach"){ var res = 0; result.foreach(res += _); res ?= (0 /: reference)(_ + _) },

			"take"               |: debug("take")(forAll { i :Int => result.take(i).toString ?= substring(0, i) }),
			"drop"               |: debug("drop")(forAll { i :Int => result.drop(i).toString ?= substring(i) }),
			"takeRight"          |: debug("takeRight")(forAll { i :Int =>
			                        	result.takeRight(i).toString ?= substring(reference.length - (i max 0))
			                        }),
			"dropRight"          |: debug("dropRight")(forAll { i :Int =>
			                        	result.dropRight(i).toString ?= substring(0, reference.length - (i max 0))
			                        }),
			"slice"              |: debug("slice")(forAll { (i :Int, j :Int) => result.slice(i, j).toString ?= substring(i, j) }),
			"splitAt"            |: debug("splitAt")(forAll { i :Int => result.splitAt(i) ?= (substring(0, i), substring(i)) }),
			"takeWhile"          |: debug("takeWhile")((result.takeWhile(_.isLetter) :Seq[Char]) ?= reference.toSeq.takeWhile(_.isLetter)),
			"dropWhile"          |: debug("dropWhile")((result.dropWhile(_.isLower) :Seq[Char]) ?= reference.toSeq.dropWhile(_.isLower)),
			"span"               |: debug("span")((result :Seq[Char]).span(_.isLower) ?= reference.toSeq.span(_.isLower)),
			"subSequence"        |: debug("subSequence")(forAll { (i :Int, j :Int) =>
			                        	if (i < 0 || j < 0 || j < i || j > reference.length)
			                        		Prop(throws(classOf[IndexOutOfBoundsException])(result.subSequence(i, j)))
			                        	else
			                        		result.subSequence(i, j).toString ?= substring(i, j)
			                        }),
			"++(Char)"           |: debug("++(Char)")(result + '!' ?= ChoppedString(reference + "!")),
			"++:(Char)"          |: debug("++:(Char)")(('!' +: result to Seq) ?= ("!" + reference to Seq)),
			"++(String)"         |: debug("++(String)")(all(Seq("", "??!").map(s => ((result ++ s :Seq[Char]) ?= (reference + s)) lbl "'" + s + "'") :_*)),
			"++:String"          |: debug("++:(String)")(all(Seq("", "??!").map(s => ((s ++: result :Seq[Char]) ?= (s + reference)) lbl "'" + s + "'") :_*)),
			"++(ChoppedString)"  |: debug("++(ChoppedString)")(((result ++ ChoppedString.empty).to(Seq) ?= reference.to(Seq)) :| "''" &&
			                            ((result ++ result).to(Seq) ?= (reference + reference to Seq)) :| "this"),
			"++:(ChoppedString)" |: debug("++:(ChoppedString)")(((ChoppedString.empty ++: result to Seq) ?= reference.to(Seq)) :| "''" &&
			                            ((result ++: result to Seq) ?= (reference + reference to Seq)) :| "this"),
			"*"                  |: debug("*")(forAll { n :Byte =>
			                            if (n < 0) Prop(throws(classOf[IllegalArgumentException]) { result * n })
			                            else result * n ?= ("" /: Iterator.fill(n)(reference)) { _ + _ }
			                        }),
			"equals"             |: debug("equals")(all(
				((result :Seq[Char]) ?= reference.toSeq) lbl ("'" + reference + "'.toSeq"),
				((result :Seq[Char]) ?= ChoppedString(reference)) lbl ("ChoppedString('" + reference + "')"),
				((result :Seq[Char]) ?= ChoppedString(result.toString)) lbl "ChoppedString(this.toString)",
				((result :Seq[Char]) ?= reference.foldLeft(ChoppedString.empty)(_ + _)) lbl "fold(empty)(toSeq)"
			)),
			"toString"           |: debug("toString")(result.toString ?= reference)
		))


	object ChoppedStringCompanionProps extends Properties("specificFactory") {
		private def include(name :String, result :ChoppedString, reference :String) :Unit =
			property(name) = props(result, reference)
//			include(new ChoppedStringProps(name)(result, reference))

		private def include(name :String)(props :Prop*) :Unit =
			property(name) = all(props :_*)
//			include((new Properties(name) /: props) { (acc, prop) => acc include prop; acc })

		include("empty", ChoppedString.empty, "")
		include("apply")(
			props("()")(ChoppedString())(""),
			props("(\"\")")(ChoppedString(""))(""),
			props("(\"boo\")")(ChoppedString("boo"))("boo"),
			props("""("you, ", "boo", " and I")""")(
				ChoppedString("you, ", "boo", " and I"))("you, boo and I")
		)
		include("implicitly[String=>ChoppedString]", "boo", "boo")
		include("from")(
			props(ChoppedString.from("boo"), "boo"),
			props("Seq()")(ChoppedString.from(Seq.empty))(""),
			props(ChoppedString.from("boo"), "boo"),
			props("ChunkedString(\"boo\")")(
				ChoppedString.from(ChoppedString("boo")))("boo"
			),
			props("\"boo\".toList")(ChoppedString.from("boo".toList))("boo"),
			props("iterator")(ChoppedString.from("boo".iterator))("boo")
		)


		include(ChoppedStringBuilderProps)

		include(new Properties("chunkedBuilder") {
			property("result()") =
				props((ChoppedString.chunkedBuilder += "you" += "boo" += "I").result(), "youbooI")
			property("clear()") = props({
				val b = ChoppedString.chunkedBuilder += "you" += "boo" += "I"
				b.clear()
				b.result()
			}, "")
			property("sizeHint") = forAll { hint :Int =>
				val b = ChoppedString.chunkedBuilder
				b sizeHint hint
				b += "you" += "boo" += "I"
				b.result().to(Seq) ?= "youbooI".to(Seq)
			}
		})

	}


	object ChoppedStringBuilderProps extends Properties("newBuilder") {
		import ChoppedString.newBuilder

		val Chunks = Gen.listOf[IterableOnce[Char]](
			Gen.oneOf(
				Arbitrary.arbitrary[List[Char]],
				Arbitrary.arbitrary[List[Char]].map(new AsIterableOnce(_)),
				Arbitrary.arbitrary[ArraySeq[Char]],
				Arbitrary.arbitrary[ArraySeq[Char]].map(new AsIterableOnce(_)),
				Arbitrary.arbitrary[String].map(_.toSeq),
				Arbitrary.arbitrary[ChoppedStringTemplate].map(_()),
			)
		)
		private def append(builder :Builder[Char, ChoppedString], chars :IterableOnce[Char]) = chars.knownSize match {
			case 1 => builder += chars.iterator.next()
			case _ => builder ++= chars
		}
		property("+=") = forAll(Gen.alphaNumStr) {
			chars :String => chars.foldLeft(newBuilder)(_ += _).result().toString ?= chars
		}
		property("++=") = forAll(Chunks) { chunks =>
			val expect  = chunks.foldLeft(new StringBuilder)(_ ++= _).result()
			val chopped = chunks.foldLeft(newBuilder)(append).result()
			chopped.toString ?= expect
		}
		property("sizeHint") = forAll(Chunks) { chunks =>
			val expect  = chunks.foldLeft(new StringBuilder)(_ ++= _).result()
			all(
				Seq(expect.length, -2, expect.length / 2, expect.length + 4).map { hint =>
					val b = newBuilder
					b sizeHint expect.length
					val chopped = chunks.foldLeft(b)(_ ++= _).result()
					(chopped.toString ?= expect) lbl
						"hint=" + hint + (if (hint == expect.length) "(correct)" else "(incorrect)")
				} :_*
			)
		}
		property("clear()") = forAll(Chunks) { chunks =>
			val builder = chunks.foldLeft(newBuilder)(append)
			builder.clear()
			builder.result().toString ?= ""
		}
	}

}
