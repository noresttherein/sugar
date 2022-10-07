package net.noresttherein.sugar.collections

import scala.collection.mutable.ArrayBuffer

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._




object ChoppedStringSpec extends Properties("ChoppedString") {
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
	include(props("+")(boo + 'I',"booI"))
	include(props("+:")('I' +: boo, "Iboo"))
	include(props("++(String)")(you ++ "boo", "youboo"))
	include(props("++:(String)")("you" ++: boo, "youboo"))
	include(props("++(ChoppedString)")(you ++ boo, "youboo"))
	include(props("++:(ChoppedString)")(you ++: boo, "youboo"))

	property("to") =
		"you and boo and I".iterator.to(ChoppedString).to(List) ?= "you and boo and I".to(List)


	object ChoppedStringCompanionProps extends Properties("companion") {
		private def include(name :String, result :ChoppedString, reference :String) :Unit =
			include(new ChoppedStringProps(name)(result, reference))

		private def include(name :String)(props :Properties*) :Unit =
			include((new Properties(name) /: props) { (acc, prop) => acc include prop; acc })

		include("empty", ChoppedString.empty, "")
		include("apply")(
			props("()")(ChoppedString(), ""),
			props("(\"\")")(ChoppedString(""), ""),
			props("(\"boo\")")(ChoppedString("boo"), "boo"),
			props("""("you, ", "boo", " and I")""")(
				ChoppedString("you, ", "boo", " and I"), "you, boo and I"
			)
		)
		include("choppedStringFromString", "boo", "boo")
		include("from")(
			props(ChoppedString.from("boo"), "boo"),
			props("Seq()")(ChoppedString.from(Seq.empty), ""),
			props(ChoppedString.from("boo"), "boo"),
			props("ChunkedString(\"boo\")")(
				ChoppedString.from(ChoppedString("boo")), "boo"
			),
			props("\"boo\".toList")(ChoppedString.from("boo".toList), "boo"),
			props("iterator")(ChoppedString.from("boo".iterator), "boo")
		)
		include(ChoppedStringBuilderProps)
		include(new Properties("chunkedBuilder") {
			include(props("result()")((ChoppedString.chunkedBuilder += "you" += "boo" += "I").result(), "youbooI"))
			include(props("clear()")({
				val b = ChoppedString.chunkedBuilder += "you" += "boo" += "I"
				b.clear()
				b.result()
			}, ""))
			property("sizeHint") = forAll { hint :Int =>
				val b = ChoppedString.chunkedBuilder
				b sizeHint hint
				b += "you" += "boo" += "I"
				b.result().to(Seq) ?= "youbooI".to(Seq)
			}
		})

	}


	def props(result :ChoppedString, reference :String) :ChoppedStringProps =
		new ChoppedStringProps(result, reference)

	def props(name :String)(result :ChoppedString, reference :String) :ChoppedStringProps =
		new ChoppedStringProps(name)(result, reference)

	class ChoppedStringProps(name :String)(result :ChoppedString, reference :String) extends Properties(name) {
		def this(result :ChoppedString, reference :String) = this("\"" + reference + "\"")(result, reference)

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
