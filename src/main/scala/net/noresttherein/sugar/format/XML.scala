package net.noresttherein.sugar.format

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}
import net.noresttherein.sugar.collections.{ChoppedString, Substring}
import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.format.XML.Tag
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt




/** A very simplistic implementation of XML marshalling and unmarshalling.
  * @author Marcin Mościcki
  */
@SerialVersionUID(ver)
class XML extends FormatAsString {

	/** Writes an opening tag of the given name and expects such a tag, possibly following whitespace.
	  * Does not handle namespaces or tag attributes - assumes everything between `<` and the next `>` is tag name.
	  */
	override val open  :Mold[String] = Open

	/** Writes a closing tag of the given name and expects such a tag, possibly following whitespace.
	  * Does not handle namespaces or tag attributes - assumes everything between `</` and the next `>` is tag name.
	  */
	override val close :Mold[String] = Close

	/** A `Mold` responsible for parsing a formatting a complete XML tag.
	  * It uses an implicit mold for `M` to format its model as `s"<$name>$model</$name>`.
	  * When parsing, it validates that input starts with an opening tag `name`,
	  * after which it reads the tag contents until the next opening or closing tag, and passes the tag body
	  * to the body mold. Finally, it validates that the remainder of the input starts with a matching closing tag
	  * Note that this means that this mold does not support recursion: `Mold[M]` cannot represent other XML tags.
	  */ //consider making it an implicit conversion
	def tagMold[M :Mold](name :String) :Mold[Tag[M]] = new TagMold(name)

	/** Parses the input until it reaches the end or anything other then text characters
	  * (i.e. until an opening or closing or some other part of the syntax).
	  * It recognizes correctly XML entities for reserved characters (but nothing else),
	  * incorporates CDATA if present XML entities and ignores all white space. Leading whitespace is ignored.
	  */
	implicit override val stringMold :Mold[String] = StringMold

	protected def propertyMold[S](propertyName :String)(implicit valueMold :Mold[S]) :Mold[S] =
		wrap(propertyName)(valueMold)

	protected override def isEmpty(liquid :ChoppedString) :Boolean = liquid.isWhitespace

	@SerialVersionUID(ver)
	private object Open extends SpecialOptBasedMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{
			var start = 0; val len = suffix.length
			while (start < len && suffix(start).isWhitespace)
				start += 1
			if (start == len || suffix(start) != '<')
				Lack
			else {
				var end = start + 1
				while (end  < len && suffix(end) != '>')
					end += 1
				if (end == len)
					Lack
				else {
					val (parsed, rest) = suffix.splitAt(end)
					Got((prefix ++ parsed, suffix.slice(start, end).toString, rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("<" + name + '>')
		override def meltOpt(name :String) = Got(melt(name))
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("<" + name + '>')
		override def appendOpt(prefix :ChoppedString, name :String) = Got(append(prefix, name))
		override def toString :String = "XML[<name>]"
	}

	@SerialVersionUID(ver)
	private object Close extends SpecialOptBasedMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{
			var start = 0; val len = suffix.length
			while (start < len && suffix(start).isWhitespace)
				start += 1
			if (start >= len - 1 || suffix(start) != '<' || suffix(start + 1) != '/')
				Lack
			else {
				var end = start + 2
				while (end  < len && suffix(end) != '>')
					end += 1
				if (end == len)
					Lack
				else {
					val (parsed, rest) = suffix.splitAt(end)
					Got((prefix ++ parsed, suffix.slice(start, end).toString, rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("</" + name + '>')
		override def meltOpt(name :String) = Got(melt(name))
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("</" + name + '>')
		override def appendOpt(prefix :ChoppedString, name :String) = Got(append(prefix, name))
		override def toString :String = "XML[</name>]"
	}



	@SerialVersionUID(ver)
	private object StringMold extends NamedMold[String] with SimpleThrowingMold[String] {
		import XMLStreamConstants._
		private[this] val factory = XMLInputFactory.newInstance()

		//todo: my own processing of XML, this is slow
		override def advance(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, String, ChoppedString) = {
			val reader = factory.createXMLStreamReader(suffix.toReader) //we could conceivably reuse the ChunkedStringReader
			var event :Int = 0
			var firstLoop = true
			var needsCopying = false           //we can just slice a suffix due to lack of anything interpreted.
			var firstText :String = null       //the first text 'event' data is stored here. Returned if builder == null.
			var builder :JStringBuilder = null //created in case of multiple text events; all contents are appended in turn.
			while (reader.hasNext && {
				event = reader.next()
				event == CHARACTERS || event == SPACE || event == COMMENT || event == CDATA
			}) {
				if ((event == COMMENT || event == CDATA) && !needsCopying && !firstLoop) {
					val offset = reader.getLocation.getCharacterOffset
					firstText = suffix.take(offset).toString
					needsCopying = true
				}
				if (event == CDATA || needsCopying && (event == CHARACTERS || event == SPACE)) {
					if (firstText == null)
						firstText = reader.getText
					else {
						if (builder == null)
							builder = new JStringBuilder(reader.getTextLength)
						builder append reader.getText
					}
				}
				firstLoop = false
			}
			val offset = reader.getLocation.getCharacterOffset
			val (skipped, remainder) = suffix.splitAt(offset)
			val res =
				if (builder != null) {
					val string = builder.toString
					(concat(prefix, skipped), string, remainder)
				} else if (firstText != null)
					(concat(prefix, skipped), firstText, remainder)
				else {
					(concat(prefix, skipped), skipped.toString, remainder)
				}
			reader.close()
			res
		}

		override def melt(model :String) :ChoppedString = ChoppedString(model)
		override def append(prefix :ChoppedString, model :String) :ChoppedString = prefix + model

		override def name = "String"
	}


	@SerialVersionUID(ver)
	private class TagMold[M](tagName :String)(implicit bodyMold :Mold[M])
		extends NamedMold[Tag[M]] with SpecialThrowingMold[Tag[M]]
	{
		override def advance(prefix :ChoppedString, suffix :ChoppedString) = {
			val (tagPrefix, _, tagSuffix) = open.advance(tagName)(prefix, suffix)
			val (bodyPrefix, body, bodySuffix) = StringMold.advance(tagPrefix, tagSuffix)
			val (_, model, modelSuffix) = bodyMold.advance(tagPrefix, ChoppedString(body))
			if (!modelSuffix.isWhitespace)
				throw ParsingException(XML.this :XML.this.type)(
					tagSuffix, "expected closing tag </" + tagName + "> after " + model + ", but got '" + modelSuffix + "'."
				)
			val (parsed, _, unparsed) = close.advance(tagName)(bodyPrefix, bodySuffix)
			(parsed, new Tag(model), unparsed)
		}
		override def append(prefix :ChoppedString, model :Tag[M]) = {
			val opened = open.append(prefix, name)
			val appended = bodyMold.append(opened, model.body)
			close.append(appended, name)
		}

		override def name :String = bodyMold match {
			case named :NamedMold[_] => "<" + tagName + ">" + named.name + "</>"
			case _ => "</" + tagName + ">"
		}
		override def toString :String = bodyMold match {
			case _ :NamedMold[_] => super[NamedMold].toString
			case _ => "<" + tagName + ">" + bodyMold + "</>"
		}
	}
	override def toString :String = "XML"
}




@SerialVersionUID(ver)
object XML extends XML {
	//todo: make it an opaque type in Scala 3
	@SerialVersionUID(ver)
	class Tag[M](val body :M) extends AnyVal

	object Tag {
		@inline def apply[M](body :M) = new Tag(body)

		@inline implicit def wrapBodyInATag[M](body :M) :Tag[M] = new Tag(body)
		@inline implicit def bodyOfATag[M](tag :Tag[M]) :M = tag.body
	}
//	/** A light wrapper over a value used to indicate on type level that parsing/formatting should use
//	  * the whole body of a tag.
//	  */
//	class Body[T](val body :T) extends AnyVal
}
