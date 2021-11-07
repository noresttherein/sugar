package net.noresttherein.slang.collection

import scala.collection.mutable.ArrayBuffer

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._




object ChunkedStringSpec extends Properties("ChunkedString") {
	val you = ChunkedString.fromString("you")
	val boo = ChunkedString.fromString("boo")
	val chunks = Seq(
		("ChunkedString.empty", ChunkedString.empty, ""),
		("ChunkedString.fromString(\"boo\")", ChunkedString.fromString("boo"), "boo"),
	)

	implicit def PropertiesToProp(properties :Properties) :Prop =
		all(properties.properties.map { case (name, prop) => prop :| name } to Seq :_*)

	private def include(name :String)(props :Properties*) :Unit =
		include((new Properties(name) /: props) { (acc, prop) => acc include prop; acc })



	include(ChunkedStringCompanionProps)
	include(props("+")(boo + 'I',"booI"))
	include(props("+:")('I' +: boo, "Iboo"))
	include(props("++(String)")(you ++ "boo", "youboo"))
	include(props("++:(String)")("you" ++: boo, "youboo"))
	include(props("++(ChunkedString)")(you ++ boo, "youboo"))
	include(props("++:(ChunkedString)")(you ++: boo, "youboo"))

	property("to") =
		"you and boo and I".iterator.to(ChunkedString).to(List) ?= "you and boo and I".to(List)


	object ChunkedStringCompanionProps extends Properties("companion") {
		private def include(name :String, result :ChunkedString, reference :String) :Unit =
			include(new ChunkedStringProps(name)(result, reference))

		private def include(name :String)(props :Properties*) :Unit =
			include((new Properties(name) /: props) { (acc, prop) => acc include prop; acc })

		include("empty", ChunkedString.empty, "")
		include("apply")(
			props("()")(ChunkedString(), ""),
			props("(\"\")")(ChunkedString(""), ""),
			props("(\"boo\")")(ChunkedString("boo"), "boo"),
			props("""("you, ", "boo", " and I")""")(
				ChunkedString("you, ", "boo", " and I"), "you, boo and I"
			)
		)
		include("fromString", ChunkedString.fromString("boo"), "boo")
		include("from")(
			props(ChunkedString.from("boo"), "boo"),
			props("Seq()")(ChunkedString.from(Seq.empty), ""),
			props(ChunkedString.from("boo"), "boo"),
			props("ChunkedString(\"boo\")")(
				ChunkedString.from(ChunkedString.fromString("boo")), "boo"
			),
			props("\"boo\".toList")(ChunkedString.from("boo".toList), "boo"),
			props("iterator")(ChunkedString.from("boo".iterator), "boo")
		)
		include(ChunkedStringBuilderProps)
		include(new Properties("chunkedBuilder") {
			include(props("result()")((ChunkedString.chunkedBuilder += "you" += "boo" += "I").result(), "youbooI"))
			include(props("clear()")({
				val b = ChunkedString.chunkedBuilder += "you" += "boo" += "I"
				b.clear()
				b.result()
			}, ""))
			property("sizeHint") = forAll { hint :Int =>
				val b = ChunkedString.chunkedBuilder
				b sizeHint hint
				b += "you" += "boo" += "I"
				b.result().to(Seq) ?= "youbooI".to(Seq)
			}
		})

	}


	def props(result :ChunkedString, reference :String) :ChunkedStringProps =
		new ChunkedStringProps(result, reference)

	def props(name :String)(result :ChunkedString, reference :String) :ChunkedStringProps =
		new ChunkedStringProps(name)(result, reference)

	class ChunkedStringProps(name :String)(result :ChunkedString, reference :String) extends Properties(name) {
		def this(result :ChunkedString, reference :String) = this("\"" + reference + "\"")(result, reference)

		property("equals")      =
			((result to Seq) ?= reference.toSeq) :| "\"" + reference + "\".toSeq" &&
				((result to Seq) ?= ChunkedString(result.toString)) :| "ChunkedString(this.toString)" :| s"\"${result.to(Seq)}\" :${result.to(Seq).getClass} ?= \"${ChunkedString(result.toString)}\" :${ChunkedString(result.toString).getClass}"&&
				((result to Seq) ?= (ChunkedString.empty /: result.toString) {
					case (acc, char) => acc ++ ChunkedString(char.toString)
				}) :| "<Char concat ChunkedString>"
		property("toString")    = result.toString ?= reference
		property("iterator")    = (ArrayBuffer.empty[Char] ++= result.iterator) ?= reference.to(ArrayBuffer)
		property("length")      = result.length ?= reference.length
		property("knownSize")   = result.knownSize ?= reference.knownSize
		property("apply")       = all((0 until reference.length).map { i => (result(i) ?= reference(i)) :| "#" + i } :_*)
		property("+(Char)")     = result + '!' ?= ChunkedString(reference + "!")
		property("+:(Char)")    = ('!' +: result to Seq) ?= ("!" + reference to Seq)
		property("++(String)")  =
			(result ++ "" ?= result) :| "''" && ((result ++ "??!" to Seq) ?= (reference + "??!" to Seq)) :| "'??!'"
		property("++:(String)") =
			("" ++: result ?= result) :| "''" && (("@" ++: result to Seq) ?= ("@" + reference to Seq)) :| "'@'"
		property("++(ChunkedString)")  =
			((result ++ ChunkedString.empty).to(Seq) ?= reference.to(Seq)) :| "''" &&
				((result ++ result).to(Seq) ?= (reference + reference to Seq)) :| "this"
		property("++:(ChunkedString)") =
			((ChunkedString.empty ++: result to Seq) ?= reference.to(Seq)) :| "''" &&
				((result ++: result to Seq) ?= (reference + reference to Seq)) :| "this"
		property("*") = forAll { n :Int =>
			if (n < 0) Prop(throws(classOf[IllegalArgumentException]) { result * n })
			else if (n > 256) Prop.passed
			else result * n ?= ("" /: Iterator.fill(n)(reference)) { _ + _ }
		}
	}

	object ChunkedStringBuilderProps extends Properties("newBuilder") {
		private def builder = ChunkedString.newBuilder ++=
			"" ++= "you" ++= Seq(',', ' ') ++= ChunkedString("boo") ++= " and ".iterator += 'I'

		property("+=")   = (ChunkedString.newBuilder += '!').result().to(Seq) ?= "!"
		property("++=")  =
			((ChunkedString.newBuilder ++= Seq('4', '2')).result() ?= "42") :| "Seq('4', '2')" &&
				((ChunkedString.newBuilder ++= "boo").result() ?= "boo") :| "\"boo\"" &&
				((ChunkedString.newBuilder ++= ChunkedString("boo")).result() ?= "boo") :| "ChunkedString(\"boo\")"
		property("result()") =
			(ChunkedString.newBuilder.result() ?= ChunkedString.empty) :| "<empty>" && {
				(builder.result().to(Seq) ?= "you, boo and I") :|
					""""", "you", Seq(',', ' '), ChunkedString("boo"), " and ".iterator, 'I'"""
			}
		property("clear()") = { val b = builder; b.clear(); b.result().to(Seq) } ?= Nil
		property("sizeHint") = forAll { hint :Int =>
			if (hint > Short.MaxValue) Prop.passed
			else {
				val b = ChunkedString.newBuilder
				b sizeHint hint
				b ++= "" ++= "you" ++= Seq(',', ' ') ++= ChunkedString("boo") ++= " and ".iterator += 'I'
				b.result().to(Seq) ?= "you, boo and I"
			}
		}
	}
}
