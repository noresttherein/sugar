package net.noresttherein.sugar.format

import scala.annotation.tailrec

import net.noresttherein.sugar.JavaTypes.JStringBuilder
import net.noresttherein.sugar.collections.{ChoppedString, StringMap, Substring}
import net.noresttherein.sugar.collections.extensions.CharJteratorExtension
import net.noresttherein.sugar.format.XML.{Tag, XMLEntities}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.{Maybe, Opt}
import net.noresttherein.sugar.vars.Opt.One




/** A very simple implementation of XML marshalling and unmarshalling.
  * Basic value types, `String` - all types for which implicit molds are declared - are parsed as untagged content.
  * User created molds composed in a ''for comprehension'' format their model as a tag of the same name
  * as the one given to its mold (or local class name, by default). The properties must occur in the exact order
  * in which they are listed in the ''for comprehension'' creating the mold, and are formatted as tags named
  * after the property (name argument passed to [[net.noresttherein.sugar.format.Format.Parts Parts]] factory method).
  * Values of properties are formatted recursively. Whitespace around values is ignored by all provided molds,
  * as well as those created by flat mapping `Parts`, but may be considered significant by other molds.
  * Note that the latter applies also to [[net.noresttherein.sugar.format.XML.stringMold stringMold]], i.e.,
  * leading and trailing whitespace in any formatted `String` is not preserved. Example:
  * {{{
  *
  *     case class BreathWeapon(damageType :String, diceCount :Int, diceType :Int)
  *     object BreathWeapon {
  *     	implicit val xmlMold :XML.Mold[BreathWeapon] = for {
  *     		weapon   <- XML[BreathWeapon]
  *     		dmg      <- weapon("damage")(_.damageType)
  *     		dice     <- weapon("dice")(_.diceCount)
  *     		diceType <- weapon("diceType")(_.diceType)
  *     	} yield BreathWeapon(dmg, dice, diceType)
  *     }
  *
  *     case class Dragon(name :String, colour :String, breath :BreathWeapon, level :Int)
  *     object Dragon {
  *     	implicit val xmlMold :XML.Mold[Dragon] = for {
  *     		dragon <- XML[Dragon]
  *     		name   <- dragon("name")(_.name)
  *     		colour <- dragon("colour")(_.colour)
  *     		breath <- dragon("breath")(_.breath)
  *     		level  <- dragon("level")(_.level)
  *     	} yield Dragon(name, colour, breath, level)
  *     }
  *
  *     XML.read("""
  *         <Dragon>
  *             <name>Firkraag</name>
  *             <colour>red</colour>
  *             <breath>
  *                 <BreathWeapon>
  *                     <damage>fire</damage>
  *                     <dice>2</dice>
  *                     <diceType>6</diceType>
  *                 </BreathWeapon>
  *             </breath>
  *         </Dragon>
  *     """)
  * }}}
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

	implicit def seqMold[M :Mold] :Mold[Seq[M]] = implicitly[Mold[M]] match {
		case named :NamedMold[M @unchecked] => new NamedSeqMold[M]()(named)
		case mold                           => new SeqMold[M]()(mold)
	}

	protected override def propertyMold[S](propertyName :String)(implicit valueMold :Mold[S]) :Mold[S] =
		wrap(propertyName)(valueMold) //this probably should override .option, .opt etc. to insert an empty tag.

	protected override def isEmpty(liquid :ChoppedString) :Boolean = liquid.isWhitespace

	@SerialVersionUID(Ver)
	private object Open extends SpecialOptBasedMold[String] with SafeMeltingMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{ //todo: ChoppedString may have very inefficient apply, migrate to usage of intIterator
			var start = 0; val len = suffix.length
			while (start < len && suffix(start).isWhitespace)
				start += 1
			if (start == len || suffix(start) != '<')
				None
			else {
				var end = start + 1
				while (end  < len && suffix(end) != '>')
					end += 1
				if (end == len)
					None
				else {
					val (parsed, rest) = suffix.splitAt(end + 1)
					One((prefix ++ parsed, suffix.substring(start + 1, end), rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("<" + name + '>')
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("<" + name + '>')
		override def toString :String = "XML.<>"
	}

	@SerialVersionUID(Ver)
	private object Close extends SpecialOptBasedMold[String] with SafeMeltingMold[String] {
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString)
				:Opt[(ChoppedString, String, ChoppedString)] =
		{ //todo: ChoppedString may have very inefficient apply, migrate to usage of intIterator
			var start = 0; val len = suffix.length
			while (start < len && suffix(start).isWhitespace)
				start += 1
			if (start >= len - 1 || suffix(start) != '<' || suffix(start + 1) != '/')
				None
			else {
				var end = start + 2
				while (end  < len && suffix(end) != '>')
					end += 1
				if (end == len)
					None
				else {
					val parsed = suffix.take(end + 1)
					val rest   = suffix.drop(end + 1)
					One((prefix ++ parsed, suffix.substring(start + 2, end), rest))
				}
			}
		}
		override def melt(name :String) = ChoppedString("</" + name + '>')
		override def append(prefix :ChoppedString, name :String) = prefix ++ ("</" + name + '>')
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

		override def advance(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, String, ChoppedString) =
			suffix match {
				case _ if suffix.isEmpty => (prefix, "", suffix)
				case Substring(string, from :Int, until :Int) => advanceSubstring(prefix, suffix, string, from, until)
				case _ => advanceJterator(prefix, suffix)
			}
		private def advanceSubstring(prefix :ChoppedString, suffix :ChoppedString, string :String, from :Int, until :Int) = {
			val res    = new JStringBuilder
			var end    = from
			while (end < until) {
				string.charAt(end) match {
					case '<' =>
						if (string.startsWith("<!--", end)) {
							end += 4
							while (end < until && !string.startsWith("-->", end))
								end += 1
						} else if (string.startsWith("<[CDATA[")) {
							end += 8
							while (end < until && !string.startsWith("]]>"))
								end += 1
						} else
							end = until
					case '&' =>
						end += 1
						val start = end
						while (end < until && string.charAt(end) != ';')
							end += 1
						if (end == until)
							throw ParsingException(XML)(suffix, "Unterminated entity in \"" + suffix + "\".")
						val entity = this.entity(string.substring(start, end - 1), suffix)
						res append entity
					case char =>
						res append char
						end += 1
				}
			}
			result(prefix, suffix, res.toString)
		}

		private def advanceJterator(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, String, ChoppedString) = {
			val length = suffix.length
			val res    = new JStringBuilder
			var state  = String
			var end    = 0
			val iter   = suffix.jterator //intIterator
			while (end < length & state != End) {
				var next = iter.next()
				state match {
					case String  => next match {
						case '<' => //Maybe a comment, maybe a CDATA section, otherwise end the content parsing.
							state = End
							if (end + 3 < length && iter.next() == '!')
								iter.next() match {
									case '-' =>
										if (iter.next() == '-') {
											state = Comment
											end  += 4
										}
									case '[' =>
										if (end + 8 < length &&
											iter.next() == 'C' && iter.next() == 'D' &&
											iter.next() == 'A' && iter.next() == 'T' &&
											iter.next() == 'A' && iter.next() == '['
										) {
											state = CDATA
											end  += 9
										}
									case _ =>
								}
						case '&' =>
							end += 1
							val entity = new JStringBuilder
							while (end < length && { next = iter.next(); next != ';' }) {
								end += 1
								entity append next
							}
							if (next != ';')
								throw ParsingException(XML)(suffix, "Unterminated entity in \"" + suffix + "\".")
							val text = this.entity(entity.toString, suffix)
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
							if (iter.next() == '-' && iter.next() == '>')
								state = String
						case _   => end += 1
					}
					case CDATA => next match {
						case ']' =>
							if (end + 2 < length)
								throw ParsingException(XML)(suffix, "Unterminated CDATA section in \"" + suffix + "\".")
							end += 3
							iter.next() match {
								case ']'  => iter.next() match {
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
			result(prefix, suffix, res.toString)
		}

		private def entity(name :String, suffix :ChoppedString) :String = {
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
			text
		}

		private def result(prefix :ChoppedString, suffix :ChoppedString, model :String) = {
			val parsed = suffix.take(model.length + 2)
			val remainder = suffix.drop(model.length + 2)
			(prefix ++ parsed, model, remainder)
		}
		override def melt(model :String) :ChoppedString = ChoppedString(model)
		override def append(prefix :ChoppedString, model :String) :ChoppedString = prefix ++ model

		override def name = "String"
	}


	@SerialVersionUID(Ver)
	private class SeqMold[M](implicit elementMold :Mold[M]) extends SpecialMold[Seq[M]] with SafeMold[Seq[M]] {
		override def advance(prefix :ChoppedString, suffix :ChoppedString) =
			if (suffix.isEmpty)
				(prefix, Seq.empty, suffix)
			else {
				val builder = Seq.newBuilder[M]
				@tailrec def rec(past :ChoppedString, next :ChoppedString) :(ChoppedString, Seq[M], ChoppedString) =
					elementMold.advanceOpt(past, next) match {
						case One((past, model, next)) => builder += model; rec(past, next)
						case _ => (past, builder.result(), next)
					}
				rec(prefix, suffix)
			}

		override def append(prefix :ChoppedString, model :Seq[M]) =
			model.map(elementMold.melt).foldLeft(prefix)(_ ++ _)

		override def toString :String = "Seq[" + elementMold + "]"
	}

	@SerialVersionUID(Ver)
	private class NamedSeqMold[M](implicit elementMold :NamedMold[M]) extends SeqMold[M] with NamedMold[Seq[M]] {
		override lazy val name = "Seq[" + elementMold.name + "]"
	}


	@SerialVersionUID(Ver)
	private class TagMold[M](tagName :String)(implicit bodyMold :Mold[M])
		extends NamedMold[Tag[M]] with SpecialThrowingMold[Tag[M]]
	{
		override def advance(prefix :ChoppedString, suffix :ChoppedString) = {
			val (tagPrefix, _, tagSuffix) = open.advance(tagName)(prefix, suffix)
//			val (bodyPrefix, body, bodySuffix) = StringMold.advance(tagPrefix, tagSuffix)
			val (bodyEnd, model, closeStart) = bodyMold.advance(tagPrefix, tagSuffix)
			val (parsed, _, unparsed) = close.advance(tagName)(bodyEnd, closeStart)
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
