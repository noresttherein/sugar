package net.noresttherein.sugar.format

import net.noresttherein.sugar.JavaTypes.{JIntIterator, JStringBuilder}
import net.noresttherein.sugar.collections.{ChoppedString, Substring}
import net.noresttherein.sugar.vars.Fallible.{Failed, Passed}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A base trait for `Format` implementations using `String` 
  * as their [[net.noresttherein.sugar.format.Format.Raw Raw]] type and
  * [[net.noresttherein.sugar.collections.ChoppedString ChoppedString]]
  * as their [[net.noresttherein.sugar.format.Format.Liquid Liquid]] type.
  * Implements all obligatory molds with the exception of [[net.noresttherein.sugar.format.Format.stringMold stringMold]].
  * In case of the latter, subclasses must decide on a cut off condition - how much of the input string is returned,
  * and possibly how to treat whitespace. Additionally, subclasses must implement only the wrapping molds, if needed:
  * [[net.noresttherein.sugar.format.Format.open open]], [[net.noresttherein.sugar.format.Format.close close]]
  * and [[net.noresttherein.sugar.format.Format.propertyMold propertyMold]].
  * @author Marcin Mościcki
  */ //todo: specialized methods and Molds, in particular taking a length argument
trait FormatAsString extends Format { format =>
	override type Raw    = String
	override type Liquid = ChoppedString

	override def melt(raw :String) :ChoppedString = ChoppedString(raw)
	override def cool(liquid :ChoppedString) :String = liquid.toString
	protected override def concat(prefix :Liquid, suffix :Liquid) :ChoppedString = prefix ++ suffix
	protected override def emptyRaw :String = ""
	protected override val emptyLiquid :ChoppedString = ChoppedString.empty

	/** Reads and writes only a single `Char`. */
	implicit override val charMold    :Mold[Char] = CharMold

	/** Uses [[java.lang.Long]]`.`[[java.lang.Long.parseLong parseLong]] to read a value (skipping leading whitespace),
	  * and appends values to the output simply using their `toString` method.
	  */
	implicit override val longMold    :Mold[Long] = LongMold

	/** Uses [[java.lang.Integer]]`.`[[java.lang.Integer.parseInt parseInt]] to read a value
	  * (skipping leading whitespace), and appends values to the output simply using their `toString` method.
	  */
	implicit override val intMold     :Mold[Int] = IntMold

	/** Uses [[java.lang.Short]]`.`[[java.lang.Short.parseShort parseShort]] to read a value
	  * (skipping leading whitespace), and appends values to the output simply using their `toString` method.
	  */
	implicit override val shortMold   :Mold[Short] = ShortMold

	/** Uses [[java.lang.Byte]]`.`[[java.lang.Byte.parseByte parseByte]] to read a value
	  * (skipping leading whitespace), and appends values to the output simply using their `toString` method.
	  */
	implicit override val byteMold    :Mold[Byte] = ByteMold

	/** Uses [[java.lang.Double]]`.`[[java.lang.Double.parseDouble parseDouble]] to read a value
	  * (skipping leading whitespace), and appends values to the output simply using their `toString` method.
	  */
	implicit override val doubleMold  :Mold[Double] = DoubleMold

	/** Uses [[java.lang.Float]]`.`[[java.lang.Float.parseFloat parseFloat]] to read a value
	  * (skipping leading whitespace), and appends values to the output simply using their `toString` method.
	  */
	implicit override val floatMold   :Mold[Float] = FloatMold

	/** Consumes only `"true"` or `"false"` (ignoring case and leading whitespace) and appends values as such. */
	implicit override val booleanMold :Mold[Boolean] = BooleanMold

	
	@SerialVersionUID(Ver)
	private object CharMold extends SpecialMold[Char] with NamedMold[Char] {
		override def name = "Char"
		override def advance(prefix :ChoppedString, suffix :ChoppedString) :(ChoppedString, Char, ChoppedString) = {
			if (suffix.isEmpty)
				parseError(this, suffix)
			val head = suffix.head
			(prefix + head, head, suffix.tail)
		}
		override def advanceOpt(prefix :ChoppedString, suffix :ChoppedString) =
			if (suffix.isEmpty)
				Lack
			else {
				val head = suffix.head
				Got((prefix + head, head, suffix.tail))
			}
		override def guardAdvance(prefix :ChoppedString, suffix :ChoppedString) =
			if (suffix.isEmpty)
				Failed(() => parseErrorMsg(this, suffix))
			else {
				val head = suffix.head
				Passed((prefix + head, head, suffix.tail))
			}

		override def append(prefix :ChoppedString, model :Char) :ChoppedString = prefix + model
		override def appendOpt(prefix :ChoppedString, model :Char) :Opt[ChoppedString] = Got(prefix + model)
		override def guardAppend(prefix :ChoppedString, model :Char) = Passed(prefix + model)
	}
	@SerialVersionUID(Ver)
	private object LongMold extends NumberMold[Long]("Long") {
		override def fromString(input :String) :Long = java.lang.Long.parseLong(input)
		override def toString(input :Long) :String = input.toString
	}
	@SerialVersionUID(Ver)
	private object IntMold extends NumberMold[Int]("Int") {
		override def fromString(input :String) :Int = java.lang.Integer.parseInt(input)
		override def toString(input :Int) :String = input.toString
	}
	@SerialVersionUID(Ver)
	private object ShortMold extends NumberMold[Short]("Short") {
		override def fromString(input :String) = java.lang.Short.parseShort(input)
		override def toString(input :Short) = input.toString
	}
	@SerialVersionUID(Ver)
	private object ByteMold extends NumberMold[Byte]("Byte") {
		override def fromString(input :String) :Byte = java.lang.Byte.parseByte(input)
		override def toString(input :Byte) = "0x" + input.toHexString
	}
	@SerialVersionUID(Ver)
	private object DoubleMold extends NumberMold[Double]("Double") {
		override def fromString(input :String) :Double = java.lang.Double.parseDouble(input)
		override def toString(input :Double) :String = input.toString
	}
	@SerialVersionUID(Ver)
	private object FloatMold extends NumberMold[Float]("Float") {
		override def fromString(input :String) :Float = java.lang.Float.parseFloat(input)
		override def toString(input :Float) :String = input.toString
	}
	@SerialVersionUID(Ver)
	private object BooleanMold extends NamedMold[Boolean] with SpecialGuardingMold[Boolean] {
		override def name = "Boolean"
		private def eos(prefix :ChoppedString) =
			Failed(() => "Expected a Boolean as String, but reached the end of input: '" + prefix + "'.")
		private def fail(suffix :ChoppedString) =
			Failed(() => "Expected a Boolean as String, but got '" + suffix + "'.")

		override def guardAdvance(prefix :ChoppedString, suffix :ChoppedString) =
			if (suffix.isEmpty)
				eos(prefix)
			else suffix match {
				case substring :Substring => parseSubstring(prefix, substring)
				case _                    => parseIterator(prefix, suffix)
			}
		private def parseSubstring(prefix :ChoppedString, suffix :Substring) = {
			val len = suffix.length
			def verify(idx :Int, lt :Char, ht :Char, lf :Char, hf :Char) :Int = {
				if (idx < len) {
					var possibility = MaybeTrue | MaybeFalse
					val char = suffix(idx)
					if (char != lt & char != ht)
						possibility &= ~MaybeTrue
					if (char != lf & char != hf)
						possibility &= ~MaybeFalse
					possibility
				} else
					0
			}
			var i = 0
			while (i < len && Character.isWhitespace(i))
				i += 1
			if (i == len)
				eos(prefix)
			else {
				val leadingWhitespace = i
				var possibility = MaybeTrue | MaybeFalse
				if ({ possibility &= ~verify(i, 't', 'T', 'f', 'F'); i += 1; possibility == 0 })
					fail(suffix)
				else if ({ possibility &= ~verify(i, 'r', 'R', 'a', 'A'); i += 1; possibility == 0 })
					fail(suffix)
				else if ({ possibility &= ~verify(i, 'u', 'U', 'l', 'L'); i += 1; possibility == 0 })
					fail(suffix)
				else if ({ possibility &= ~verify(i, 'e', 'E', 's', 'S'); i += 1; possibility == 0 })
					fail(suffix)
				else if ((possibility & MaybeTrue) == MaybeTrue) {
					val parsed = suffix.slice(leadingWhitespace, i)
					val remainder = suffix.drop(i)
					Passed((prefix ++ parsed, true, remainder))
				} else if ({ possibility &= ~verify(i, '_', '_', 'e', 'E'); i += 1; possibility == 0 })
					fail(suffix)
				else {
					val parsed = suffix.slice(leadingWhitespace, i)
					val remainder = suffix.drop(i)
					Passed((prefix ++ parsed, false, remainder))
				}
			}
		}
		private def parseIterator(prefix :ChoppedString, suffix :ChoppedString) = {
			def verify(next :Int, lt :Char, ht :Char, lf :Char, hf :Char) :Int = {
				val char = next.toChar
				var possibility = MaybeTrue | MaybeFalse
				if (char != lt & char != ht)
					possibility &= ~MaybeTrue
				if (char != lf & char != hf)
					possibility &= ~MaybeFalse
				possibility
			}
			var leadingWhitespace = 0
			val i = suffix.jiterator
			var char = i.nextInt().toChar
			while (i.hasNext && Character.isWhitespace(char)) {
				char = i.nextInt().toChar
				leadingWhitespace += 1
			}
			var possibility = MaybeTrue | MaybeFalse
			if (char != 't' & char != 'T')
				possibility &= ~MaybeTrue
			if (char != 'f' & char != 'F')
				possibility &= ~MaybeFalse
			if (possibility == 0)
				fail(suffix)
			if ({ 
				possibility &= ~verify(char, 't', 'T', 'f', 'F')
				possibility == 0 || !i.hasNext 
			})
				fail(suffix)
			else if ({ 
				possibility &= ~verify(i.nextInt(), 'r', 'R', 'a', 'A')
				possibility == 0 || !i.hasNext
			})
				fail(suffix)
			else if ({
				possibility &= ~verify(i.nextInt(), 'u', 'R', 'l', 'L')
				possibility == 0 || !i.hasNext
			})
				fail(suffix)
			else if ({possibility &= ~verify(i.nextInt(), 'e', 'E', 's', 'S'); possibility == 0 })
				fail(suffix)
			else if ((possibility & MaybeTrue) == MaybeTrue) {
				val (parsed, remainder) = suffix.splitAt(leadingWhitespace + 4)
				Passed((prefix ++ parsed, true, remainder))
			} else if (!i.hasNext || {
				possibility &= ~verify(i.next(), '_', '_', 'e', 'E')
				possibility == 0 
			})
				fail(suffix)
			else {
				val (parsed, remainder) = suffix.splitAt(leadingWhitespace + 5)
				Passed((prefix ++ parsed, false, remainder))
			}
		}

		override def guardAppend(prefix :ChoppedString, model :Boolean) =
			Passed(if (model) prefix ++ "true" else prefix ++ "false")
	}
	private final val MaybeTrue  = 1
	private final val MaybeFalse = 2

	
	private abstract class NumberMold[S](override val name :String)
		extends SpecialThrowingMold[S] with NamedMold[S]
	{
		def fromString(input :String) :S
		def toString(input :S) :String

		override def advance(prefix :ChoppedString, suffix :ChoppedString) = {
			def error =
				throw ParsingException(FormatAsString.this :FormatAsString.this.type)(
					suffix, "Expected a Long, got an empty String following '" + prefix + "'."
				)
			val numberString = suffix match {
				case _ if suffix.isEmpty =>
					""
				case Substring(string, from, until) =>
					var start = from
					while (start < until && string.charAt(start).isWhitespace)
						start += 1
					if (start == until)
						error
					gulpString(string, start, until)
				case _ => //we have suffix.nonEmpty
					val it = suffix.intIterator
					var last :Char = 0
					var wasWhitespace :Boolean = true
					while (it.hasNext && {
						last = it.nextInt().toChar
						wasWhitespace = last.isWhitespace
						wasWhitespace
					}) {}
					if (wasWhitespace)
						error
					gulpIterator(last, it)
			}
			val res = fromString(numberString)
			val (parsed, unparsed) = suffix.splitAt(numberString.length)
			(prefix ++ parsed, res, unparsed)
		}

		override def append(prefix :ChoppedString, model :S) :ChoppedString =
			if (model == null) prefix ++ "null" else prefix ++ toString(model)

		private def gulpString(string :String, from :Int, until :Int) :String = {
			var end = from
			if ({ val char = string.charAt(from); char == '-' | char == '+' })
				end += 1
			if (isNaN(string, end, until))
				string.substring(from, end + 3)
			else if (isInfinity(string, end, until))
				string.substring(from, end + 8)
			else if (isNull(string, end, until))
				string.substring(from, end + 4)
		    else {
				val isHex = this.isHex(string, end, until)
				if (isHex)
					end += 2
				//skip leading zeros
				while (end < until && string.charAt(end) == '0')
					end += 1
				//skip all digits before dot or exponent
				end = skip(isHex, string, end, until)
				if (end < until) {
					if (string.charAt(end) == '.') {
						end = skip(isHex, string, end + 1, until)
					if ({ val old = end; end = skipExp(string, end, until); old != end })
						end = skip(string, end + 1, until)(isDigit)
					}
				}
				string.substring(from, end)
		    }
		}
		private def gulpIterator(first :Char, it :JIntIterator) :String =
			if (!it.hasNext)
				if (first.isDigit) first.toString else ""
			else if (first == 'I' | first == 'i') {
				var i = 1
				while (i < 8 && it.hasNext && {
					val char = it.nextInt(); char == "INFINITY".charAt(i) || char == "infinity".charAt(i)
				}) i += 1
				if (i == 8) "Infinity" else ""
			} else if (first == 'N' | first == 'n') { //todo: parse null
				if ({
					var char = it.nextInt()
					char == 'A' | char == 'a' && it.hasNext && {
						char = it.nextInt(); char == 'N' | char == 'n'
					}
				})
					"NaN"
				else
                    ""
			} else {
				val res = new JStringBuilder
				var char = first
				if (first == '-' | first == '+') {
					res append first
					char = it.nextInt().toChar
				}
				val isHex =
					char == '0' && it.hasNext && {
						res append '0'
						char = it.nextInt().toChar
						char == 'X' | char == 'x'
					}
				if (!it.hasNext)
					(res append char).toString
				else {
					if (isHex) {
						res append char //char == 'X' || 'x'
						char = it.nextInt().toChar
						char = skip(char, it, res)(this.isHex)
					} else {
						while (char == '0' && { res append char; it.hasNext })
							char = it.nextInt().toChar
						char = skip(char, it, res)(isDigit)
					}
					if (char == '.' && it.hasNext) {
						res append char
						char = it.nextInt().toChar
						char = skip(isHex, char, it, res)
					}
					if (char == 'E' | char == 'e' && it.hasNext) {
						res append char
						char = it.nextInt().toChar
						if (char == '-' | char == '+' && it.hasNext) {
							res append char
							char = it.nextInt().toChar
						}
						char = skip(char, it, res)(isDigit)
					}
					res.toString
				}
			}
		private def isNaN(string :CharSequence, from :Int, until :Int) :Boolean = {
			var char :Char = 0
			until >= from + 3 &&
				{ char = string.charAt(from); char == 'N' | char == 'n' } &&
				{ char = string.charAt(from + 1); char == 'A' | char == 'a' } &&
				{ char = string.charAt(from + 2); char == 'N' | char == 'n' }
		}
		private def isNull(string :CharSequence, from :Int, until :Int) :Boolean = {
			var char :Char = 0
			until >= from + 4 &&
				{ char = string.charAt(from); char == 'N' | char == 'n' } &&
				{ char = string.charAt(from + 1); char == 'U' | char == 'u' } &&
				{ char = string.charAt(from + 2); char == 'L' | char == 'l' } &&
				{ char = string.charAt(from + 3); char == 'L' | char == 'l' }
		}
		private def isInfinity(string :CharSequence, from :Int, until :Int) :Boolean = {
			var char :Char = 0
			var i = from
			until >= from + 8 &&
				{ char = string.charAt(i); i += 1; char == 'I' | char == 'i' } &&
				{ char = string.charAt(i); i += 1; char == 'N' | char == 'n' } &&
				{ char = string.charAt(i); i += 1; char == 'F' | char == 'f' } &&
				{ char = string.charAt(i); i += 1; char == 'I' | char == 'i' } &&
				{ char = string.charAt(i); i += 1; char == 'N' | char == 'n' } &&
				{ char = string.charAt(i); i += 1; char == 'I' | char == 'i' } &&
				{ char = string.charAt(i); i += 1; char == 'T' | char == 't' } &&
				{ char = string.charAt(i); i += 1; char == 'Y' | char == 'y' }
		}
		private def isHex(string :CharSequence, from :Int, until :Int) :Boolean =
			until >= from + 2 &&
				string.charAt(from) == '0' && { val char = string.charAt(from + 1); char == 'X' | char == 'x' }

		private def isHex(char :Int) :Boolean =
			char >= 'a' & char <= 'f' || char >= 'A' & char <= 'F' || char >= '0' & char <= '9' || char == '_'
		private def isDigit(char :Int) :Boolean = Character.isDigit(char.toChar)

		private def skip(string :CharSequence, from :Int, until :Int)(p :Int => Boolean) :Int = {
			var i = from
			while (i < until && p(string.charAt(i)))
				i += 1
			i
		}
		private def skip(first :Char, it :JIntIterator, res :JStringBuilder)(p :Int => Boolean) :Char = {
			var char :Int = first
			while (p(char) && { res append char.toChar; it.hasNext })
				char = it.nextInt()
			char.toChar
		}
		@inline private def skip(isHex :Boolean, string :CharSequence, from :Int, until :Int) :Int =
			if (isHex) skip(string, from, until)(this.isHex)
			else skip(string, from, until)(isDigit)

		@inline private def skip(isHex :Boolean, first :Char, it :JIntIterator, res :JStringBuilder) :Char =
			if (isHex) skip(first, it, res)(this.isHex)
			else skip(first, it, res)(isDigit)

		private def skipExp(string :CharSequence, from :Int, until :Int) :Int =
			if (from == until)
				from
			else {
				var end = from
				var char = string.charAt(from)
				if (char == 'E' | char == 'e') {
					end += 1
					if (end < until && { char = string.charAt(end); char == '+' | char == '-' })
						end += 1
				}
				end
			}
	}

}
