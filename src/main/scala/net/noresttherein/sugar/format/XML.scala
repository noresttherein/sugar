package net.noresttherein.sugar.format

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}

import net.noresttherein.sugar.collections.{ChoppedString, StringMap, Substring}
import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.constants.MaxExceptionMessageLength
import net.noresttherein.sugar.format.XML.{Tag, XMLEntities}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Opt




/** A very simplistic implementation of XML marshalling and unmarshalling.
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
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

	protected override def propertyMold[S](propertyName :String)(implicit valueMold :Mold[S]) :Mold[S] =
		wrap(propertyName)(valueMold) //this probably should override .option, .opt etc. to insert an empty tag.

	protected override def isEmpty(liquid :ChoppedString) :Boolean = liquid.isWhitespace

	@SerialVersionUID(Ver)
	private object Open extends SpecialOptBasedMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{ //todo: ChoppedString may have very inefficient apply, migrate to usage of intIterator
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
					val (parsed, rest) = suffix.splitAt(end + 1)
					Got((prefix ++ parsed, suffix.substring(start + 1, end), rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("<" + name + '>')
		override def meltOpt(name :String) = Got(melt(name))
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("<" + name + '>')
		override def appendOpt(prefix :ChoppedString, name :String) = Got(append(prefix, name))
		override def toString :String = "XML.<>"
	}

	@SerialVersionUID(Ver)
	private object Close extends SpecialOptBasedMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{ //todo: ChoppedString may have very inefficient apply, migrate to usage of intIterator
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
					val parsed = suffix.take(end + 1)
					val rest   = suffix.drop(end + 1)
					Got((prefix ++ parsed, suffix.substring(start + 2, end), rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("</" + name + '>')
		override def meltOpt(name :String) = Got(melt(name))
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("</" + name + '>')
		override def appendOpt(prefix :ChoppedString, name :String) = Got(append(prefix, name))
		override def toString :String = "XML.</>"
	}



	@SerialVersionUID(Ver)
	private object StringMold extends NamedMold[String] with SimpleThrowingMold[String] {
		import java.lang.{String=>JString}
//		private[this] val factory = XMLInputFactory.newInstance()

		private final val Comment = 1
		private final val CDATA   = 2
		private final val String  = 4
		private final val End     = 5

		override def advance(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, String, ChoppedString) = {
			//todo: check if suffix has a fast apply and consider using it instead of the iterator.
			val length = suffix.length
			val iter   = suffix.intIterator
			val res    = new JStringBuilder
			var state  = String
			var end    = 0
			while (end < length & state != End) {
				var next = iter.next().toChar
				state match {
					case String  => next match {
						case '<' => //Maybe a comment, maybe a CDATA section, otherwise end the content parsing.
							state = End
							if (end + 3 < length && iter.next().toChar == '!')
								iter.next().toChar match {
									case '-' =>
										if (iter.next().toChar == '-') {
											state = Comment
											end  += 4
										}
									case '[' =>
										if (end + 8 < length &&
											iter.next().toChar == 'C' && iter.next().toChar == 'D' &&
											iter.next().toChar == 'A' && iter.next().toChar == 'T' &&
											iter.next().toChar == 'A' && iter.next().toChar == '['
										) {
											state = CDATA
											end  += 9
										}
									case _ =>
								}
						case '&' =>
							end += 1
							val entity = new JStringBuilder
							while (end < length && { next = iter.next().toChar; next != ';' }) {
								end += 1
								entity append next
							}
							if (next != ';')
								throw ParsingException(XML)(suffix, "Unterminated entity in \"" + suffix + "\".")
							val name = entity.toString
							val text =
								if (name.length == 5 && name.charAt(0) == '#') {
									try JString.valueOf(Integer.parseInt(name.substring(1)).toChar) catch {
										case e :NumberFormatException =>
											throw ParsingException(XML)(
												suffix, "Illegal numeric character reference: &" + name + ";", e
											)
									}
								} else if (name.length == 6 && name.charAt(0) == '#' && name.charAt(1) == 'x') {
									try JString.valueOf(Integer.parseInt(name.substring(2), 16).toChar) catch {
										case e :NumberFormatException =>
											throw ParsingException(XML)(
												suffix, "Illegal numeric character reference: &" + name + ";", e
											)
									}
								} else
									XMLEntities.getOrElse(name, null)
							if (text == null)
								throw ParsingException(XML)(
									suffix, "Unrecognized entity in \"" + suffix + "\": " + name + "."
								)
							res append text
						case _ =>
							res append next
							end += 1
					}
					case Comment => next match {
						case '-' =>
							if (end + 2 >= length)
								throw ParsingException(XML)(suffix, "Unterminated comment in \"" + suffix + "\".")
							end += 3
							if (iter.next().toChar == '-' && iter.next().toChar == '>')
								state = String
						case _   => end += 1
					}
					case CDATA => next match {
						case ']' =>
							if (end + 2 < length)
								throw ParsingException(XML)(suffix, "Unterminated CDATA section in \"" + suffix + "\".")
							end += 3
							iter.next().toChar match {
								case ']'  => iter.next().toChar match {
									case '>'  => state = String
									case char => res append ']' append ']' append char
								}
								case char => res append ']' append char
							}
						case _   =>
							res append next
							end += 1
					}
				}
			}
			state match {
				case Comment => throw ParsingException(XML)(suffix, "Unterminated comment in \"" + suffix + "\".")
				case CDATA   =>	throw ParsingException(XML)(suffix, "Unterminated CDATA section in \"" + suffix + "\".")
				case _ =>
			}
			val string    = res.toString
			val parsed    = suffix.take(end)
			val remainder = suffix.drop(end)
			(prefix :++ parsed, string, remainder)
		}
/*
//		import XMLStreamConstants._
		def erroradvance(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, String, ChoppedString) =
			try {
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
			} catch {
				case e :Exception =>
					throw ParsingException(XML)(suffix, e.getMessage, e)
			}
*/
		override def melt(model :String) :ChoppedString = ChoppedString(model)
		override def append(prefix :ChoppedString, model :String) :ChoppedString = prefix ++ model

		override def name = "String"
	}


	@SerialVersionUID(Ver)
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




@SerialVersionUID(Ver)
object XML extends XML {
	//todo: make it an opaque type in Scala 3
	@SerialVersionUID(Ver)
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

	private val XMLEntities = StringMap(
		"amp"    -> "&",
		"AMP"    -> "&",
		"apos"   -> "'",
		"quot"   -> "\"",
		"dollar" -> "$",
		"excl"   -> "!",
		"quest"  -> "?",
//		"comma"  -> ",",
//		"period" -> ".",
		"colon"  -> ":",
		"semi"   -> ";",
		"sol"    -> "/",
		"num"    -> "#",
//		"prcnt"  -> "%",
		"lt"     -> "<",
		"LT"     -> "<",
		"gt"     -> ">",
		"GT"     -> ">",
		"equals" -> "=",
//		"lpar"   -> "(",
//		"rpar"   -> ")",
//		"ast"    -> "*",
//		"midast" -> "*",
//		"plus"   -> "+",
	)
}
