package net.noresttherein.sugar.collections

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala

import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter




object ChoppedStringSpec extends Properties("ChoppedString") {

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(200)

	implicit val choppedStringGenerator :Arbitrary[ChoppedString] =
		Arbitrary(Arbitrary.arbitrary[Seq[Char]].map(_ to ChoppedString))

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
		forAll { chops :List[String] => prop(ChoppedString(chops), chops.foldLeft("")(_ + _)) } &&
			forAll { (chops :List[String], from :Int, until :Int) =>
				prop(ChoppedString(chops).slice(from, until), substring(from, until)(chops.foldLeft("")(_ + _)))
			}

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
	property("iterator")           = prop { (chops, expect) => chops.iterator.to(ArrayBuffer) ?= expect.to(ArrayBuffer) }
	property("intIterator")        = prop { (chops, expect) => chops.intIterator.asScala.to(Seq) ?= expect.map(_.toInt) }
	property("javaIterator")       = prop { (chops, expect) => chops.jiterator.asScala.toSeq ?= expect.map(_.toInt) }

	property("isWhitespace")       = prop { (chops, expect) => chops.isWhitespace ?= expect.forall(_.isWhitespace) }

	property("foreach")            = prop { (chops, expect) =>
	                                  var res = 0; chops.foreach(res += _)
	                                  res ?= (0 /: expect)(_ + _)
	                                 }

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

	property("+(Char)")            = prop { (chops, expect) =>
	                                 	forAll { char :Char => props(chops + char, expect + char) }
	                                 }
	property("+:(Char)")           = prop { (chops, expect) =>
	                                  forAll { char :Char => props(char +: chops, char.toString + expect) }
	                                 }
	property("++(String)")         = prop { (chops, expect) => forAll { s :String => props(chops ++ s, expect + s) } }
	property("++:(String)")        = prop { (chops, expect) => forAll { s :String => props(s ++: chops, s + expect) } }
	property("++(ChoppedString)")  = prop { (chops, expect) => forAll { s :String => props(s ++: chops, s + expect) } }
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
		props("\"" + reference + "\"")(result)(reference)

	def props(name :String)(result :ChoppedString)(implicit reference :String) =
		(result ?= reference) && (name |: all(
			"length"             |: (result.length ?= reference.length),
			"knownSize"          |: (result.knownSize ?= reference.knownSize),
			"head"               |: (if (reference.length == 0) Prop(throws(classOf[NoSuchElementException])(result.head))
			                        else (result.head ?= reference(0))),
			"last"               |: (if (reference.length == 0) Prop(throws(classOf[NoSuchElementException])(result.last))
			                        else (result.last ?= reference(reference.length - 1))),
			"apply"              |: all((0 until reference.length).map(i => (result(i) ?= reference(i)) :| "#" + i):_*),
			"iterator"           |: ((ArrayBuffer.empty[Char] ++= result.iterator) ?= reference.to(ArrayBuffer)),
			"stepper"            |: (result.stepper.javaIterator.asScala.to(ArrayBuffer) ?=
			                            reference.map(Integer.valueOf(_)).to(ArrayBuffer)),
			"intIterator"        |: (result.intIterator.asScala.to(IndexedSeq) ?= reference.map(_.toInt)),
			"javaIterator"       |: (result.jiterator.asScala.toSeq ?= reference.map(_.toInt)),

			"isWhitespace"       |: (result.isWhitespace ?= reference.forall(_.isWhitespace)),

			"foreach"            |: { var res = 0; result.foreach(res += _); res ?= (0 /: reference)(_ + _) },

			"take"               |: forAll { i :Int => result.take(i).toString ?= substring(0, i) },
			"drop"               |: forAll { i :Int => result.drop(i).toString ?= substring(i) },
			"takeRight"          |: forAll { i :Int =>
			                        	result.takeRight(i).toString ?= substring(reference.length - (i max 0))
			                        },
			"dropRight"          |: forAll { i :Int =>
			                        	result.dropRight(i).toString ?= substring(0, reference.length - (i max 0))
			                        },
			"slice"              |: forAll { (i :Int, j :Int) => result.slice(i, j).toString ?= substring(i, j) },
			"splitAt"            |: forAll { i :Int => result.splitAt(i) ?= (substring(0, i), substring(i)) },
			"takeWhile"          |: ((result.takeWhile(_.isLetter) :Seq[Char]) ?= reference.toSeq.takeWhile(_.isLetter)),
			"dropWhile"          |: ((result.dropWhile(_.isLower) :Seq[Char]) ?= reference.toSeq.dropWhile(_.isLower)),
			"span"               |: ((result :Seq[Char]).span(_.isLower) ?= reference.toSeq.span(_.isLower)),
			"subSequence"        |: forAll { (i :Int, j :Int) =>
			                        	if (i < 0 || j < 0 || j < i || j > reference.length)
			                        		Prop(throws(classOf[IndexOutOfBoundsException])(result.subSequence(i, j)))
			                        	else
			                        		result.subSequence(i, j).toString ?= substring(i, j)
			                        },
			"++(Char)"           |: (result + '!' ?= ChoppedString(reference + "!")),
			"++:(Char)"          |: (('!' +: result to Seq) ?= ("!" + reference to Seq)),
			"++(String)"         |: all(Seq("", "??!").map(s => ((result ++ s :Seq[Char]) ?= (reference + s)) :| "'" + s + "'") :_*),
			"++:String"          |: all(Seq("", "??!").map(s => ((s ++: result :Seq[Char]) ?= (s + reference)) :| "'" + s + "'") :_*),
			"++(ChoppedString)"  |: (((result ++ ChoppedString.empty).to(Seq) ?= reference.to(Seq)) :| "''" &&
			                            ((result ++ result).to(Seq) ?= (reference + reference to Seq)) :| "this"),
			"++:(ChoppedString)" |: (((ChoppedString.empty ++: result to Seq) ?= reference.to(Seq)) :| "''" &&
			                            ((result ++: result to Seq) ?= (reference + reference to Seq)) :| "this"),
			"*"                  |: forAll { n :Int =>
			                            if (n < 0) Prop(throws(classOf[IllegalArgumentException]) { result * n })
			                            else if (n > 256) Prop.passed
			                            else result * n ?= ("" /: Iterator.fill(n)(reference)) { _ + _ }
			                        },
			"equals"             |: all(
				((result :Seq[Char]) ?= reference.toSeq) :| ("'" + reference + "'.toSeq"),
				((result :Seq[Char]) ?= ChoppedString(reference)) :| ("ChoppedString('" + reference + "')"),
				((result :Seq[Char]) ?= ChoppedString(result.toString)) :| "ChoppedString(this.toString)",
				((result :Seq[Char]) ?= reference.foldLeft(ChoppedString.empty)(_ + _)) :| "fold(empty)(toSeq)"
			),
			"toString"           |: (result.toString ?= reference)
		))


	object ChoppedStringCompanionProps extends Properties("companion") {
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
		include("choppedStringFromString", "boo", "boo")
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


//	def props(result :ChoppedString, reference :String) :Prop =
//		new ChoppedStringProps(result, reference)

//	def props(name :String)(result :ChoppedString, reference :String) :Prop =
//		new ChoppedStringProps(name)(result, reference)

/*
	class ChoppedStringProps(name :String)(result :ChoppedString, reference :String) extends Properties(name) {
		def this(result :ChoppedString, reference :String) = this("\"" + reference + "\"")(result, reference)

		private def substring(from :Int, until :Int = reference.length) =
			reference.substring(from max 0, until min reference.length min (from max 0))

		property("take")        = forAll { i :Int => result.take(i).toString ?= substring(0, i) }
		property("drop")        = forAll { i :Int => result.drop(i).toString ?= substring(i) }
		property("takeRight")   = forAll { i :Int =>
			result.takeRight(i).toString ?= substring(reference.length - (i max 0))
		}
		property("dropRight")   = forAll { i :Int => result.dropRight(i).toString ?= substring(0, i) }
		property("slice")       = forAll { (i :Int, j :Int) => result.slice(i, j).toString ?= substring(i, j) }
		property("splitAt")     = forAll { i :Int => result.splitAt(i) ?= (substring(0, i), substring(i)) }
		property("takeWhile")   = (result.takeWhile(_.isLetter) :Seq[Char]) ?= reference.toSeq.takeWhile(_.isLetter)
		property("dropWhile")   = (result.dropWhile(_.isLower) :Seq[Char]) ?= reference.toSeq.dropWhile(_.isLower)
		property("span")        = (result :Seq[Char]).span(_.isLower) ?= reference.toSeq.span(_.isLower)
		property("subSequence") = forAll { (i :Int, j :Int) => result.subSequence(i, j).toString ?= substring(i, j) }

		property("isWhitespace") = result.isWhitespace ?= reference.forall(_.isWhitespace)

		property("equals")      =
			((result to Seq) ?= reference.toSeq) :| "\"" + reference + "\".toSeq" &&
				((result to Seq) ?= ChoppedString(result.toString)) :| "ChunkedString(this.toString)" :| s"\"${result.to(Seq)}\" :${result.to(Seq).getClass} ?= \"${ChoppedString(result.toString)}\" :${ChoppedString(result.toString).getClass}"&&
				((result to Seq) ?= (ChoppedString.empty /: result.toString) {
					case (acc, char) => acc ++ ChoppedString(char.toString)
				}) :| "<Char concat ChunkedString>"
		property("toString")    = result.toString ?= reference
		property("iterator")    = (ArrayBuffer.empty[Char] ++= result.iterator) ?= reference.to(ArrayBuffer)
		property("length")      = result.length ?= reference.length
		property("knownSize")   = result.knownSize ?= reference.knownSize
		property("apply")       = all((0 until reference.length).map { i => (result(i) ?= reference(i)) :| "#" + i } :_*)
		property("+(Char)")     = result + '!' ?= ChoppedString(reference + "!")
		property("+:(Char)")    = ('!' +: result to Seq) ?= ("!" + reference to Seq)
		property("++(String)")  =
			(result ++ "" ?= result) :| "''" && ((result ++ "??!" to Seq) ?= (reference + "??!" to Seq)) :| "'??!'"
		property("++:(String)") =
			("" ++: result ?= result) :| "''" && (("@" ++: result to Seq) ?= ("@" + reference to Seq)) :| "'@'"
		property("++(ChunkedString)")  =
			((result ++ ChoppedString.empty).to(Seq) ?= reference.to(Seq)) :| "''" &&
				((result ++ result).to(Seq) ?= (reference + reference to Seq)) :| "this"
		property("++:(ChunkedString)") =
			((ChoppedString.empty ++: result to Seq) ?= reference.to(Seq)) :| "''" &&
				((result ++: result to Seq) ?= (reference + reference to Seq)) :| "this"
		property("*") = forAll { n :Int =>
			if (n < 0) Prop(throws(classOf[IllegalArgumentException]) { result * n })
			else if (n > 256) Prop.passed
			else result * n ?= ("" /: Iterator.fill(n)(reference)) { _ + _ }
		}
	}
*/

	object ChoppedStringBuilderProps extends Properties("newBuilder") {
		private def builder = ChoppedString.newBuilder ++=
			"" ++= "you" ++= Seq(',', ' ') ++= ChoppedString("boo") ++= " and ".iterator += 'I'

		property("+=")   = (ChoppedString.newBuilder += '!').result().to(Seq) ?= "!"
		property("++=")  =
			((ChoppedString.newBuilder ++= Seq('4', '2')).result() ?= "42") :| "Seq('4', '2')" &&
				((ChoppedString.newBuilder ++= "boo").result() ?= "boo") :| "\"boo\"" &&
				((ChoppedString.newBuilder ++= ChoppedString("boo")).result() ?= "boo") :| "ChunkedString(\"boo\")"
		property("result()") =
			(ChoppedString.newBuilder.result() ?= ChoppedString.empty) :| "<empty>" && {
				(builder.result().to(Seq) ?= "you, boo and I") :|
					""""", "you", Seq(',', ' '), ChunkedString("boo"), " and ".iterator, 'I'"""
			}
		property("clear()") = { val b = builder; b.clear(); b.result().to(Seq) } ?= Nil
		property("sizeHint") = forAll { hint :Int =>
			if (hint > Short.MaxValue) Prop.passed
			else {
				val b = ChoppedString.newBuilder
				b sizeHint hint
				b ++= "" ++= "you" ++= Seq(',', ' ') ++= ChoppedString("boo") ++= " and ".iterator += 'I'
				b.result().to(Seq) ?= "you, boo and I"
			}
		}
	}
}
