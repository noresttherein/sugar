package net.noresttherein.slang.matching

import java.util.regex.Pattern
import java.lang.Character.{MAX_CODE_POINT, MIN_CODE_POINT}

import net.noresttherein.slang.matching.RX.{Alternative, AtLeast, Concatenation, Flag, FlaggedGroup, NamedGroup, QuantifiedRX, RepeatedRX, RXGroup}

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex



/** A regular expression and a building block providing syntax for creating larger regular expressions.
  * While it can be used to match input in a fashion similar to scala [[scala.util.matching.Regex Regex]]
  * and java [[java.util.regex.Pattern Pattern]] classes as expected, the focus is on providing a more readable way
  * of constructing large regular expression than the standard string representation, partly due to whitespace separators
  * typical to code and partly due to more friendly names for some character classes. It should prevent many typo-like
  * errors, but the strict correctness was sacrificed for flexibility of embedding fragments given as string patterns
  * (through the [[net.noresttherein.slang.matching.RX$.apply RX(pattern)]] method) wherever this would improve readability.
  *
  * @see [[net.noresttherein.slang.matching.RX$ RX object]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
sealed abstract class RX extends Serializable {

	/** A regular expression matching this or another expression: `this|other`. */
	def ||(other :RX) :RX = this | other

	/** A regular expression matching this or another expression: `this|other`. */
	def |(other :RX) :RX = new Alternative(this, other)



	/** Concatenates two regular expressions: `firstthis`.
	  * @return a regular expression matching `first`, followed by `this`.
	  */
	def ::(first :RX) :RX = new Concatenation(first, this)

	/** Puts a comment after this regular expression. The comment is made in a separate group flagged with 'x'.
	  * This does not enable comments for the rest of the expression.
	  * @return this expression followed by a comment expression with the given string: `this(?x:#$comment\n)`
	  */
	def ##(comment :String) :RX = this :: RX.comment(comment)



	/** A regular expression greedily matching this expression at most once: `(this)?`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  * @see [[net.noresttherein.slang.matching.RX#?? ??]]
	  * @see [[net.noresttherein.slang.matching.RX#?+ ?+]]
	  */
	def ? :RX = new RepeatedRX(ncgroup, "?")

	/** A regular expression greedily matching this expression zero or more times: `(this)*`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  * @see [[net.noresttherein.slang.matching.RX#*? *?]]
	  * @see [[net.noresttherein.slang.matching.RX#*+ *+]]
	  */
	def * :RX = new RepeatedRX(ncgroup, "*")

	/** A regular expression greedily matching this expression one or more times: `(this)+`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  * @see [[net.noresttherein.slang.matching.RX#+? +?]]
	  * @see [[net.noresttherein.slang.matching.RX#++ ++]]
	  */
	def + :RX = new RepeatedRX(ncgroup, "+")

	/** A regular expression greedily matching this expression at least the given number of times: `(this){min,}`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  * @param min a wrapper over an integer which can be constructed through a
	  *            [[net.noresttherein.slang.matching.RX.RepeatAtLeast.-* -*]] method available by an implicit conversion:
	  *            `alpha * 3.-*`.
	  */
	def *(min :AtLeast) :RX = atLeast(min.n)

	/** A regular expression greedily matching this expression at least `min` but at most `max` times: `(this){min,max}`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  */
	def *(min :Int, max :Int) :RX = between(min, max)

	/** A regular expression greedily matching this expression exactly the given number of times: `(this){n}`.
	  * Will match the longest fragment of input possible before backtracking; this may lead to omitting some matches
	  * which could be found using reluctant matching.
	  */
	def *(n :Int) :RX = times(n)



	/** A regular expression reluctantly matching this expression at most once: `(this)??`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  * @see [[net.noresttherein.slang.matching.RX#? ?]]
	  * @see [[net.noresttherein.slang.matching.RX#?+ ?+]]
	  */
	def ?? :RX = new RepeatedRX(ncgroup, "??")

	/** A regular expression reluctantly matching this expression at zero or more times: `(this)*?`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  * @see [[net.noresttherein.slang.matching.RX#* *]]
	  * @see [[net.noresttherein.slang.matching.RX#*+ *+]]
	  */
	def *? :RX = new RepeatedRX(ncgroup, "*?")

	/** A regular expression reluctantly matching this expression one or more times: `(this)+?`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  * @see [[net.noresttherein.slang.matching.RX#+ +]]
	  * @see [[net.noresttherein.slang.matching.RX#++ ++]]
	  */
	def +? :RX = new RepeatedRX(ncgroup, "+?")

	/** A regular expression reluctantly matching this expression at least the given number of times: `(this){min,}?`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  * @param min a wrapper over an integer which can be constructed through a
	  *            [[net.noresttherein.slang.matching.RX.RepeatAtLeast.-* -*]] method available by an implicit conversion:
	  *            `alpha *? 3.-*`.
	  */
	def *?(min :AtLeast) :RX = atLeast(min.n, "?")

	/** A regular expression reluctantly matching this expression at least `min` but at most `max` times: `(this){min,max}?`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  */
	def *?(min :Int, max :Int) :RX = between(min, max, "?")

	/** A regular expression reluctantly matching this expression exactly the given number of times: `(this}{n}?`.
	  * Will attempt to match the shortest fragment of input possible; this may lead to finding more matches than
	  * with greedy matching.
	  */
	def *?(n :Int) :RX = times(n, "?")



	/** A regular expression possessively matching this expression at most once: `(this)?+`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  * @see [[net.noresttherein.slang.matching.RX#? ?]]
	  * @see [[net.noresttherein.slang.matching.RX#?? ??]]
	  */
	def ?+ :RX = new RepeatedRX(ncgroup, "?+")

	/** A regular expression possessively matching this expression zero or more times: `(this)*+`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  * @see [[net.noresttherein.slang.matching.RX#* *]]
	  * @see [[net.noresttherein.slang.matching.RX#*? *?]]
	  */
	def *+ :RX = new RepeatedRX(ncgroup, "*+")

	/** A regular expression possessively matching this expression zero or more times: `(this)++`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  * @see [[net.noresttherein.slang.matching.RX#+ +]]
	  * @see [[net.noresttherein.slang.matching.RX+? +?]]
	  */
	def ++ :RX = new RepeatedRX(ncgroup, "++")

	/** A regular expression possessively matching this expression at least the given number of times: `(this}{min,}+`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  * @param min a wrapper over an integer which can be constructed through a
	  *            [[net.noresttherein.slang.matching.RX.RepeatAtLeast.-* -*]] method available by an implicit conversion:
	  *            `alpha *+ 3.-*`.
	  */
	def *+(min :AtLeast) :RX = atLeast(min.n, "+")

	/** A regular expression possessively matching this expression at least `min` but at most `max` times: `(this){min,max}+`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  */
	def *+(min :Int, max :Int) :RX = between(min, max, "+")

	/** A regular expression possessively matching this expression exactly the given number of times: `(this){n}+`.
	  * Will attempt to match the longest fragment of input possible and, once successful, will not backtrack
	  * even if the following fragment can't be matched. May not find a match where other methods would, but can
	  * be significantly faster.
	  */
	def *+(n :Int) :RX = times(n, "+")



	private def times(n :Int, symbol :String = "") :RX =
		if (n < 0)
			throw new IllegalArgumentException("Illegal regexp group quantifier: {" + n +",}")
		else new QuantifiedRX(ncgroup, n, n, symbol)

	private def atLeast(min :Int, symbol :String = "") :RX =
		if (min < 0)
			throw new IllegalArgumentException("Illegal regexp group quantifier: {" + min + '}')
		else new QuantifiedRX(ncgroup, min, -1, symbol)

	private def between(min :Int, max :Int, symbol :String = "") :RX =
		if (min < 0 || max < 0 || max < min)
			throw new IllegalArgumentException("Illegal regexp group quantifiers: {" + min + ',' + max + '}')
		else new QuantifiedRX(ncgroup, min, max, symbol)




	/** A regular expression which matches this expression without consuming any input (look-ahead): `(?=this)`. */
	def ~> :RX = new RXGroup("=", this)

	/** A regular expression which matches this expression against already consumed input (look-behind): `(?&lt;=this)`. */
	def <~ :RX = new RXGroup("<=", this)

	/** A regular expression matching when this expression doesn't match and which does not consume input
	  * (negative look-ahead): `(?!this)`. */
	def !~> :RX = new RXGroup("!", this)

	/** A regular expression matching when this expression doesn't match the preceding input (negative look-behind): `(?&lt;!this)`. */
	def <~! :RX = new RXGroup("<!", this)

	/** Turns on possessive matching for this expression: `(?&gt;this)`. It will try to match the longest fragment of input
	  * possible and, once matched, will prevent backtracking even if the following part of the expression does not match. */
	def possessive :RX = new RXGroup(">", this)



	/** Turns the given flags on/off for matching this expression only: `(?idmsux-idmsux:this)`*/
	def apply(flags :Flag*) :RX = new FlaggedGroup(this, flags)



	/** A named, capturing group matching this expression: `(?&lt;name&gt;this)`. Matched fragment can be referenced with
	  * [[net.noresttherein.slang.matching.RX.group RX.group(name)]].
	  */
	def group(name :String) :RX = new NamedGroup(name, this)


	private[matching] def ncgroup :RX = new RXGroup(this)

	/** Names of all capturing groups in this regular expression. */
	def groups :Seq[String] = Nil

	private[matching] def listGroups(acc :Seq[String]) :Seq[String] =
		if (acc.isEmpty) groups
		else groups ++: acc





	/** Matches this expression followed by any single character: `this.`. */
	def `.` :RX = this :: RX.`.`

	/** Matches this expression followed by any single character: `this.`. */
	def any :RX = this :: RX.`.`

	/** Matches this expression followed by the 'tab' character: `this\t`. */
	def `\\t` :RX = this :: RX.`\\t`

	/** Matches this expression followed by the 'tab' character: `this\t`. */
	def TAB :RX = this :: RX.TAB

	/** Matches this expression followed by the 'space' character: `this\x20`. */
	def ` ` :RX = this :: RX.` `

	/** Matches this expression followed by the 'space' character: `this\x20`. */
	def space :RX = this :: RX.space

	/** Matches this expression followed by the carriage return character: `this\r`. */
	def `\\r` :RX = this :: RX.`\\r`

	/** Matches this expression followed by the carriage return character: `this\r`. */
	def CR :RX = this :: RX.CR

	/** Matches this expression followed by the new line character: `this\n`. */
	def `\\n` :RX = this :: RX.`\\n`

	/** Matches this expression followed by the new line character: `this\n`. */
	def NL :RX = this :: RX.NL

	/** Matches this expression followed by the form feed character: `this\f`. */
	def `\\f` :RX = this :: RX.`\\f`

	/** Matches this expression followed by the form feed character: `this\f`. */
	def FF :RX = this :: RX.FF

	/** Matches this expression followed by any valid line separator character sequence: `this\R`. */
	def `\\R` :RX = this :: RX.`\\R`

	/** Matches this expression followed by any valid line separator character sequence: `this\R`. */
	def BR :RX = this :: RX.BR

	/** Matches this expression followed by the 'esc' character: `this\e`. */
	def ESC :RX = this :: RX.ESC

	/** Matches this expression followed by any ASCII control character. */
	def Ctrl :RX = this :: RX.Ctrl

	/** Matches this expression followed by any control character (Unicode character class). */
	def uCtrl :RX = this :: RX.uCtrl

	/** Matches this expression followed by the alarm/bell character: `this\a`. */
	def `\\a` :RX = this :: RX.`\\a`

	/** Matches this expression followed by the alarm/bell character: `this\a`. */
	def Bell :RX = this :: RX.Bell

	/** Matches this expression followed by any ASCII whitespace character: `this\s`. */
	def `\\s` :RX = this :: RX.`\\s`

	/** Matches this expression followed by any whitespace character (Unicode character class). */
	def WS :RX = this :: RX.WS

	/** Matches this expression followed by the 'space' or 'tab' character. */
	def Blank :RX = this :: RX.Blank

	/** Matches this expression followed by any horizontal whitespace character: `this\h`. */
	def `\\h` :RX = this :: RX.`\\h`

	/** Matches this expression followed by any horizontal whitespace character: `this\h`. */
	def HS :RX = this :: RX.HS

	/** Matches this expression followed by any vertical whitespace character: `this\v`. */
	def `\\v` :RX = this :: RX.`\\v`

	/** Matches this expression followed by any vertical whitespace character: `this\v`. */
	def VS :RX = this :: RX.VS



	/** Matches this expression followed by any ASCII character: `this[\x00-\x7F`. */
	def ASCII :RX = this :: RX.ASCII

	/** Matches this expression followed by any decimal digit. */
	def Digit :RX = this :: RX.Digit

	/** Matches this expression followed by any decimal digit. */
	def D :RX = this :: RX.D

	/** Matches this expression followed by any digit (Unicode character class). */
	def uDigit :RX = this :: RX.uDigit

	/** Matches this expression followed by any valid octal digit (`[0-7]`). */
	def OCT :RX = this :: RX.OCT

	/** Matches this expression followed by any valid hexadecimal digit (`[0-9a-zA-Z]`). */
	def HEX :RX = this :: RX.HEX

	/** Matches this expression followed by any Latin upper case letter. */
	def AZ :RX = this :: RX.AZ

	/** Matches this expression followed by any Unicode upper case letter (using the Unicode character class). */
	def uAZ :RX = this :: RX.uAZ

	/** Matches this expression followed by any Latin lower case letter. */
	def az :RX = this :: RX.az

	/** Matches this expression followed by any Unicode lower case letter (using the Unicode character class). */
	def uaz :RX = this :: RX.uaz

	/** Matches this expression followed by any Latin letter. */
	def Az :RX = this :: RX.Az

	/** Matches this expression followed by any Latin letter. */
	def Alpha :RX = this :: RX.Alpha

	/** Matches this expression followed by any Unicode letter. */
	def uAz :RX = this :: RX.uAz

	/** Matches this expression followed by any Unicode letter. */
	def uAlpha :RX = this :: RX.uAlpha

	/** Matches this expression followed by any ASCII letter or digit. */
	def AlNum :RX = this :: RX.AlNum

	/** Matches this expression followed by any Unicode letter or digit. */
	def uAlNum :RX = this :: RX.uAlNum

	/** Matches this expression followed by any ASCII punctuation character. */
	def Punct :RX = this :: RX.Punct

	/** Matches this expression followed by any Unicode punctuation character (using the Unicode character class). */
	def uPunct :RX = this :: RX.uPunct

	/** Matches this expression followed by any visible ASCII character. */
	def Graph :RX = this :: RX.Graph

	/** Matches this expression followed by any visible Unicode character. */
	def uGraph :RX = this :: RX.uGraph

	/** Matches this expression followed by any printable ASCII character. */
	def Prnt :RX = this :: RX.Prnt

	/** Matches this expression followed by any printable Unicode character. */
	def uPrnt :RX = this :: RX.uPrnt

	/** Matches this expression followed by any 'word'/'identifier' ASCII character: `[a-zA-Z0-9_]`. */
	def W :RX = this :: RX.W

	/** Matches this expression followed by any Unicode 'word' character. */
	def uW :RX = this :: RX.uW


	/** Matches this expression followed by the end of input (or the end of any line of input, depending on the flags set): `this$`. */
	def `$` :RX = this :: RX.`$`

	/** Matches this expression followed by the end of line: `(?m:this$)`. */
	def EOL :RX = this :: RX.EOL

	/** Matches this expression followed by the end of input, except for any ending line terminators (`\Z`). */
	def `\\Z` :RX = this :: RX.`\\Z`

	/** Matches this expression followed by the end of input, except for any ending line terminators (`\Z`). */
	def $$ :RX = this :: RX.$$

	/** Matches this expression followed by the end of input (`\z`). */
	def `\\z` :RX = this :: RX.`\\z`

	/** Matches this expression followed by the end of input (`\z`). */
	def EOF :RX = this :: RX.EOF


	/** Matches this expression followed by a word boundary (`\b`). */
	def `\\b` :RX = this :: RX.`\\b`

	/** Matches this expression followed by a word boundary (`\b`). */
	def / :RX = this :: RX./




	def unapplySeq(char :Char) :Option[Seq[Char]] = regex.unapplySeq(char)

	def unapplySeq(string :CharSequence) :Option[Seq[String]] = regex.unapplySeq(string)

	def unapplySeq(m :Regex.Match) :Option[Seq[String]] = regex.unapplySeq(m)

	def pattern :Pattern = regex.pattern

	def compile :Regex = regex

	private[this] lazy val regex = toString.r(groups : _*)



	private[matching] def appendTo(res :StringBuilder) :Unit

	override def toString :String = {
		val res = new StringBuilder
		appendTo(res)
		res.toString
	}


	def canEqual(that :Any) :Boolean = that.isInstanceOf[RX]

	override def equals(that :Any) :Boolean = that match {
		case rx :RX => (this eq rx) || rx.canEqual(this) && toString == rx.toString
		case _ => false
	}

	override def hashCode :Int = toString.hashCode

}






object RX {


	/** Implicit conversion from an interpolated `String` (`StringContext`) providing factory methods for creating
	  * `RX` instances. methods from this class can be used using the string interpolation syntax: `p"Digit"`.
	  * Note that the interpreted string is treated as raw: standard java escape sequences such as `\n` are not
	  * substituted with the appropriate characters; only the unicode escapes are inlined.
	  */
	implicit class RXInterpolation(val sc :StringContext) extends AnyVal {

		/** Creates a regular expression [[net.noresttherein.slang.matching.RX RX]] object from an interpolated ''raw''
		  * String. Standard java literal escapes for special characters like '\\', '\n' and '\t' are left verbatim.
		  * In particular, any escape sequences defined by [[java.util.regex.Pattern]] don't need double escaping.
		  * Unicode literals however a eagerly processed before passing to the regular expression engine.
		  * The '$' literal can be inserted by doubling it: rx"$$".
		  */
		def rx(args :Any*) :RX = RX(sc.raw(args : _*))

		/** A regular expression matching a single character from the posix class/Unicode character category
		  * of the name specified by the following `String`.
		  */
		def p(args :Any*) :CharacterClass = posix("", args)

		/** A regular expression matching a single character from the Unicode script of the name specified by the following `String`. */
		def script(args :Any*) :CharacterClass = posix("Is", args)

		/** A regular expression matching a single character from the Unicode block of the name specified by the following `String`. */
		def blk(args :Any*) :CharacterClass = posix("In", args)

		/** A regular expression matching a single character with the Unicode property of the name specified by
		  * the following `String`. This can be also used to match Unicode categories.
		  */
		def prop(args :Any*) :CharacterClass = posix("Is", args)

		/** A regular expression matching a single ASCII character specified by its code in the octal format.
		  * The following string must consist only of valid octal digits and represent a value in the `[0..255]` range.
		  */
		def o(args :Any*) :CharacterClass = RX.oct(sc.raw(args :_*))

		/** A regular expression matching a single ASCII character specified by its code in the hexadecimal format.
		  * The following string must consist only of valid hexadecimal digits and represent a value in the `[0..255]` range.
		  */
		def x(args :Any*) :CharacterClass = RX.hex(sc.raw(args :_*))

		/** A regular expression matching a single Unicode character specified by its code in the hexadecimal format.
		  * The following string must consist only of valid hexadecimal digits and represent a value
		  * in the `[0..MAX_CODE_POINT]` range.
		  */
		def X(args :Any*) :CharacterClass = RX.Hex(sc.raw(args :_*))


		private def posix(prefix :String, args :Seq[Any]) :CharacterClass =
			new AdapterCharacterClass("\\p{" + prefix + sc.raw(args :_*) + '}')

		private[RX] def cls(args :Any*) :CharacterClass = new AdapterCharacterClass(sc.raw(args:_*))

	}





	/** Adapts a regular expression given as a string to an [[net.noresttherein.slang.matching.RX RX]] instance.
	  * @param pattern a valid regular expression as accepted by [[java.util.regex.Pattern Pattern]]'s `compile` method.
	  * @param groups optional names for capturing groups inside the expression which can be used with the scala
	  *               [[scala.util.matching.Regex Regex]] API.
	  * @return a regular expression object which can be composed with other instances to form larger expressions.
	  */
	def apply(pattern :String, groups :String*) :RX = new AdapterRX(pattern, groups)

	/** A regular expression matching an empty string. Useful mainly as a starting point. */
	def apply() :RX = EmptyRX


	/** A back-reference for a group captured by a preceding fragment of a regular expression as created by the
	  * [[net.noresttherein.slang.matching.RX#group(name:String) RX.group]] method.
	  * @param name the name of the referenced group.
	  * @return a regular expression matching exactly the fragment matched by the specified group.
	  */
	def group(name :String) :RX = apply("\\k<" + name + '>')


	/** An ignored regular expression (matching an empty string) containing the given comment in a separate
	  * appropriately flagged group.
	  */
	def comment(comment :String) :RX = RX("#" + comment + '\n')(Comments)





	/** A regular expression matching empty string. */
	final val EmptyRX :RX = new EmptyRX

	/** A regular expression matching any single character (`.`). */
	final val `.` :RX = new AtomicRX(".")

	/** A regular expression matching any single character (`.`). */
	final val any = `.`



	/** A regular expression matching any ASCII control character. */
	final val Ctrl = cls"""\x00-\x1F\x7F"""

	/** A regular expression matching any control character (Unicode character class). */
	final val uCtrl = prop"Control"

	/** A regular expression matching the 'esc' character (`\e`). */
	final val `\\e` = \('e')

	/** A regular expression matching the 'esc' character (`\e`). */
	final val ESC = `\\e`

	/** A regular expression matching the alarm/bell character. */
	final val `\\a` :RX = \('a')

	/** A regular expression matching the alarm/bell character. */
	final val Bell = `\\a`


	/** A regular expression matching the carriage return character (`\r`). */
	final val `\\r` = \('r')

	/** A regular expression matching the carriage return character (`\r`). */
	final val CR = `\\r`

	/** A regular expression matching the new line character (`\n`). */
	final val `\\n` = \('n')

	/** A regular expression matching the new line character (`\n`). */
	final val NL = `\\n`

	/** A regular expression matching the form feed character (`\f`). */
	final val `\\f` = \('f')

	/** A regular expression matching the form feed character (`\f`). */
	final val FF = `\\f`

	/** A regular expression matching any valid line separator character sequence. */
	final val `\\R` = \('R')

	/** A regular expression matching any valid line separator character sequence. */
	final val BR = `\\R`

	/** A regular expression matching the 'tab' character (`\t`). */
	final val `\\t` = \('t')

	/** A regular expression matching the 'tab' character (`\t`). */
	final val TAB = `\\t`

	/** A regular expression matching the 'space' character (' ') using a character class (and not the literal). */
	final val ` ` = cls"""\x20"""

	/** A regular expression matching the 'space' character (' ') using a character class (and not the literal). */
	final val space = ` `

	/** A regular expression matching any ASCII whitespace character. */
	final val `\\s` = \('s')

	/** A regular expression matching any ASCII whitespace character. */
	final val WS = `\\s`

	/** A regular expression matching any whitespace character (Unicode character class). */
	final val uWS = prop"White_Space"

	/** A regular expression matching the 'space' and 'tab' characters. */
	final val Blank = TAB | space

	/** A regular expression matching any horizontal whitespace character. */
	final val `\\h` = \('h')

	/** A regular expression matching any horizontal whitespace character (`\h`). */
	final val HS = `\\h`

	/** A regular expression matching any vertical whitespace character. */
	final val `\\v` = \('v')

	/** A regular expression matching any vertical whitespace character. */
	final val VS = `\\v`



	/** A regular expression matching any ASCII character (`[\x00-\x7F`). */
	final val ASCII = cls"""\x00-\x7F"""

	/** A regular expression matching any decimal digit (`\d`). */
	final val `\\d` = \('d')

	/** A regular expression matching any decimal digit. */
	final val Digit = '0' to '9' :CharacterClass

	/** A regular expression matching any decimal digit. */
	final val D = Digit

	/** A regular expression matching any digit (Unicode character class). */
	final val uDigit = prop"Digit"

	/** A regular expression matching any digit (Unicode character class). */
	final val uD = uDigit

	/** A regular expression matching any valid octal digit (`[0-7]`). */
	final val OCT = '0' to '7' :CharacterClass

	/** A regular expression matching any valid hexadecimal digit (`[0-9a-zA-Z]`). */
	final val HEX = Digit | ('a' to 'f') | ('A' to 'F')



	/** A regular expression matching any Latin upper case letter. */
	final val AZ = 'A' to 'Z' :CharacterClass

	/** A regular expression matching any Unicode upper case letter (using the Unicode character class). */
	final val uAZ = prop"Uppercase"

	/** A regular expression matching any Latin lower case letter. */
	final val az = 'a' to 'z' :CharacterClass

	/** A regular expression matching any Unicode lower case letter (using the Unicode character class). */
	final val uaz = prop"Lowercase"

	/** A regular expression matching any Latin letter. */
	final val Alpha = az | AZ

	/** A regular expression matching any Latin letter. */
	final val Az = Alpha

	/** A regular expression matching any Unicode letter. */
	final val uAlpha = prop"Alphabetic"

	/** A regular expression matching any Unicode letter. */
	final val uAz = uAlpha

	/** A regular expression matching any ASCII letter or digit. */
	final val AlNum = Alpha | ('0' to '9')

	/** A regular expression matching any Unicode letter or digit. */
	final val uAlNum = uAlpha | uDigit



	/** A regular expression matching any ASCII punctuation character. */
	final val Punct = adapt("""]\[{}\\!"#$%&'()*+,./:;<=>?@^_`|~-""")

	/** A regular expression matching any Unicode punctuation character (using the Unicode character class). */
	final val uPunct = prop"Punctuation"

	/** A regular expression matching any visible ASCII character. */
	final val Graph = AlNum | Punct

	/** A regular expression matching any visible Unicode character. */
	final val uGraph = !adapt("""\p{IsWhite_Space}\p{gc=Cc}\p{gc=Cs}\p{gc=Cn}""")

	/** A regular expression matching any printable ASCII character. */
	final val Prnt = Graph | x"20"

	/** A regular expression matching any printable Unicode character. */
	final val uPrnt = (uGraph | Blank) && !uCtrl

	/** A regular expression matching any 'word'/'identifier' ASCII character: `[a-zA-Z0-9_]`. */
	final val W = AlNum | '_'

	/** A regular expression matching any Unicode word character. */
	final val uW = adapt("""\p{Alpha}\p{gc=Mn}\p{gc=Me}\p{gc=Mc}\p{Digit}\p{gc=Pc}\p{IsJoin_Control}""")





	/** A regular expression turning on the `UNIX_LINES` ('d') flag for the reminder of the expression.
	  * This will treat only the new line character ([[net.noresttherein.slang.matching.RX$.NL NL]] = '\n')
	  * as a valid line terminators, turning off the default behavior of accepting line terminator sequences from any
	  * operating system. Can be turned off by `!UnixEOL` or applied only to a particular expression fragment: `regexp(UnixEOL)`.
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val UnixEOL = new Flag("d")

	/** A regular expression turning on the `DOTALL` ('s') flag for the reminder of the expression.
	  * It will make the special '.' character ([[net.noresttherein.slang.matching.RX$.`.` `.`]]) match also the
	  * line terminator characters.
	  * Can be turned off by `!DotEOL` or applied only to a particular expression fragment: `regexp(DotEOL)`.
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val DotEOL = new Flag("s")

	/** A regular expression turning on the `MULTILINE` ('m') flag for the reminder of the expression.
	  * This will make the special characters `'^'` ([[net.noresttherein.slang.matching.RX.`^` `^`]]) and `'$'`
	  * ([[net.noresttherein.slang.matching.RX$.`$` `$`]]) match a beginning and end of any line of input, respectively.
	  * By default they match only the beginning and end of whole input.
	  * Can be turned off by `!MultiLine` or applied only to a particular expression fragment: `regexp(MultiLine)`.
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val MultiLine = new Flag("m")

	/** A regular expression turning on the `CASE_INSENSITIVE` ('i') flag for the reminder of the expression.
	  * This will make the matching case insensitive for only the ASCII characters.
	  * Can be turned off by `!NoCase` or applied only to a particular expression fragment: `regexp(NoCase)`.
	  * @see [[net.noresttherein.slang.matching.RX$.uCase uCase]]
	  * @see [[net.noresttherein.slang.matching.RX$.uNoCase uNoCase]]
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val NoCase = new Flag("i")

	/** A regular expression turning on the `CASE_INSENSITIVE` and `UNICODE_CASE` ('iu') flags for the reminder of
	  * the expression. This will make the matching case insensitive, following the Unicode specification.
	  * Can be turned off by `!uNoCase` or applied only to a particular expression fragment: `regexp(uNoCase)`.
	  * @see [[net.noresttherein.slang.matching.RX$.NoCase NoCase]]
	  * @see [[net.noresttherein.slang.matching.RX$.uCase uCase]]
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val uNoCase = new Flag("iu")

	/** A regular expression turning on the `UNICODE_CASE` ('u') flag for the reminder of the expression.
	  * When used in conjunction with the [[net.noresttherein.slang.matching.RX$.NoCase NoCase]] ('i') flag,
	  * case recognition will be in accordance with the Unicode specification. It does not make the matching
	  * case insensitive on its own.
	  * Can be turned off by `!uCase` or applied only to a particular expression fragment: `regexp(NoCase, uCase)`.
	  * @see [[net.noresttherein.slang.matching.RX$.uNoCase uNoCase]]
	  * @see [[net.noresttherein.slang.matching.RX$.NoCase NoCase]]
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  */
	final val uCase = new Flag("u")

	/** A regular expression turning on the `UNICODE_CHARACTER_CLASS` ('U') flag for the reminder of the expression.
	  * This makes the standard POSIX character classes (`\p{Digit}`, `\p{Lower}`, etc.) behave according to the
	  * Unicode specification rather than ASCII only; it also implies the 'u'/`UNICODE_CASE` flag
	  * ([[net.noresttherein.slang.matching.RX$.uCase uCase]]). POSIX character classes are not used by the `RX`
	  * implementation, so it is essentially equivalent to `RX.uCase`.
	  * Can be turned off by `!Unicode`, but cannot be applied only to a particular expression group.
	  * @see [[net.noresttherein.slang.matching.RX$.uCase uCase]]
	  * @see [[net.noresttherein.slang.matching.RX$.uNoCase uNoCase]]
	  * @see [[net.noresttherein.slang.matching.RX$.NoCase NoCase]]
	  */
	final val Unicode = new Flag("U")

	/** A regular expression turning on the `COMMENTS` ('x') flag for the reminder of the expression.
	  * This will ignore any whitespace characters in the pattern as well as any sequence starting with the '#' character
	  * until the end of line. It is useful only when embedding literal strings as regular expressions and does not
	  * affect any `RX` value provided here (which use character classes and escapes to match whitespace).
	  * Can be turned off by `!Comments` or applied only to a particular expression fragment: `regexp(Comments)`.
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:RX.Flag*) RX.apply]]
	  * @see [[net.noresttherein.slang.matching.RX.## RX.##]]
	  * @see [[net.noresttherein.slang.matching.RX.comment comment]]
	  */
	final val Comments = new Flag("x")



	/** A non-consuming regular expression matching the beginning of input (or the beginning of any line of input,
	  * depending on the flags set).
	  */
	final val `^` = RX("^")

	/** A non-consuming regular expression matching the end of input (or the end of any line of input,
	  * depending on the flags set).
	  */
	final val `$` = RX("$")

	/** A non-consuming regular expression matching the end of line (`$`). */
	final val EOL = `$`(MultiLine)

	/** A non-consuming regular expression matching the start of input (`\A`). */
	final val `\\A` = RX("\\A")

	/** A non-consuming regular expression matching the start of input (`\A`). */
	final val ^^ = `\\A`

	/** A non-consuming regular expression matching the end of input, except for any ending line terminators (`\Z`). */
	final val `\\Z` = RX("\\Z")

	/** A non-consuming regular expression matching the end of input, except for any ending line terminators (`\Z`). */
	final val $$ = `\\Z`

	/** A regular expression matching the end of input, including any ending line terminators (`\z`). */
	final val `\\z` = RX("\\z")

	/** A regular expression matching the end of input, including any ending line terminators (`\Z`). */
	final val EOF = `\\Z`

	/** A non-consuming regular expression matching a word boundary (`\b`). */
	final val `\\b` = RX("\\b")

	/** A non-consuming regular expression matching a word boundary (`\b`). */
	final val / = `\\b`



	/** Standard character classes complementing other character classes. Provided for completeness,
	  * but extracted from the main `RX` scope to avoid additional confusion. As they follow the pattern
	  * of uppercasing the negated character class (i.e., `\s` stands for 'any whitespace'
	  * and `\S` - anything but whitespace) the difference may not be apparent at first glance.
	  * Prefer negating the base character class instead : `!``\\s``` or `!WS`.
	  */
	object complements {
		/** Any character but horizontal whitespace. */
		final val `\\H` = \('H')

		/** Any character but vertical whitspace. */
		final val `\\V` = \('V')

		/** Any non-whitespace character. */
		final val `\\S` = \('S')

		/** Any non-word (ASCII) character. */
		final val `\\W` = \('W')

		/** Any character other than decimal digit. */
		final val `\\D` = \('D')

		/** Zero-length expression matching regions not being word boundaries. */
		final val `\\B` = \('B')

	}





	/** A regular expression matching a character from the UTF8 8-bit range specified by its code in the octal format.
	  * @return a regular expression using the `\0xxx` notation to match the specified character.
	  */
	def oct(octal :String) :CharacterClass = {
		java.lang.Integer.parseInt(octal, 8)
		@inline def invalid = throw new IllegalArgumentException("Not an octal character code: \"" + octal + "\"")

		def validate(digit :Char) :Unit =
			if (digit > '7' || digit < '0')
				invalid

		octal.length match {
			case n if n == 0 || n > 3 =>
				invalid
			case 3 =>
				if (octal.charAt(0) > '3' || octal.charAt(0) < '0')
					invalid
				validate(octal charAt 1)
				validate(octal charAt 2)
			case _ =>
				octal foreach validate
		}
		atom("\\0" + octal)
	}

	/** A regular expression matching a character from the UTF8 8-bit range specified by its code.
	  * @return a regular expression using the `\0...` notation to match the specified character.
	  */
	def oct(value :Int) :CharacterClass =
		if (value < 0 || value > 255)
			throw new IllegalArgumentException("Invalid character code: " + value)
		else
			atom("\\0" + value.toOctalString)

	/** A regular expression matching a character from the UTF8 8-bit range specified by its code in the hexadecimal format.
	  * @param hexadecimal a string of length in the `[1-2]` range consisting only of valid hexadecimal digits.
	  * @return a regular expression using the `\x..` notation to match the specified character.
	  */
	def hex(hexadecimal :String) :CharacterClass =
		if (hexadecimal.length > 2)
			throw new IllegalArgumentException("Invalid character code: \"" + hexadecimal + "\"")
		else Hex(hexadecimal)

	/** A regular expression matching a character from the UTF8 8-bit range specified by its code.
	  * @param codePoint the code of the matched character in the `[0-255]` range.
	  * @return a regular expression using the `\x..` notation to match the specified character.
	  */
	def hex(codePoint :Int) :CharacterClass =
		if (codePoint < 0 || codePoint > 255)
			throw new IllegalArgumentException("Invalid character code: " + codePoint)
		else if (codePoint < 16)
		     atom("\\x0" + codePoint.toHexString)
		else
			atom("\\x" + codePoint.toHexString)

	/** A regular expression matching a given Unicode character specified by its code in the hexadecimal format.
	  * @param hexadecimal a non-empty string consisting only of valid hexadecimal digit representing an integer
	  *                    from the [[0-MAX_CODE_POINT]] range.
	  * @return a regular expression using the `\x..` or `\\u....` notation to match the specified character.
	  */
	def Hex(hexadecimal :String) :CharacterClass = {
		def validate(digit :Char) :Unit =
			if (!(digit >= '0' && digit <= '9' || digit >= 'a' && digit <= 'f' || digit >= 'A' && digit <= 'F'))
				throw new IllegalArgumentException("Not a hexadecimal character code: \"" + hexadecimal + "\"")
		hexadecimal foreach validate

		hexadecimal.length match {
			case n if n <= 0 =>
				throw new IllegalArgumentException("Not a hexadecimal character code: \"" + hexadecimal + "\"")
			case 1 =>
				atom("\\x0" + hexadecimal)
			case 2 =>
				atom("\\x" + hexadecimal)
			case 4 =>
				atom("\\u" + hexadecimal)
			case _ =>
				val code = java.lang.Integer.parseInt(hexadecimal, 16)
				if (code < MIN_CODE_POINT || code > MAX_CODE_POINT)
					throw new IllegalArgumentException("Not a hexadecimal character code: \"" + hexadecimal + "\"")
				atom("\\x{" + hexadecimal + '}')
		}
	}

	/** A regular expression matching a given Unicode character specified by its code.
	  * @param codePoint an integer from the [[0-MAX_CODE_POINT]] range.
	  * @return a regular expression using the `\x..` or `\\u...` notation to match the specified character.
	  */
	def Hex(codePoint :Int) :CharacterClass = Hex(codePoint.toHexString)



	private def \(escape :Char) :CharacterClass = new AtomicCharClass("\\" + escape)


	/** A zero-length regular expression turning on a particular flag changing the behaviour of the Regex engine.
	  * When used on its own, it takes effect from the point of placement until turned off with the negated flag `!flag`
	  * or the end of expression. Additionally, any number of flags can be applied to an arbitrary regular expression
	  * turning it into a non-capturing flagged group: `RX(NoCase)`. Flags applied this way will affect only that expression.
	  * Supported values are listed as constants in the `RX` object:
	  *   - UnixEOL
	  *   - DotEOL
	  *   - NoCase
	  *   - uNoCase
	  *   - uCase
	  *   - Unicode
	  *   - Multiline
	  *
	  * @param code the letter (or letters) used to represent the flag inside a regular expression
	  * @param isOn whether the flag is being turned on or off.
	  * @see [[net.noresttherein.slang.matching.RX.apply(flags:Flag*) RX.apply]]
	  */
	final class Flag private[RX](val code :String, val isOn :Boolean = true)
		extends RXGroup(if (isOn) "-" + code else code, EmptyRX)
	{
		/** Switches this flag between 'on' and 'off'. */
		def unary_! :Flag = new Flag(code, !isOn)
	}



	/** An implicit `RX` implementation matching the given string literal. Any characters otherwise interpreted
	  * by the regular expression engine are escaped.
	  */
	implicit class StringLiteral(literal :String) extends RX {
		private[this] val escaped = Pattern.quote(literal)

		override private[matching] def appendTo(res :StringBuilder) :Unit = res append escaped
	}



	private class EmptyRX extends RX {

		override def ::(other :RX) :RX = other

		override def ? :RX = this
		override def * :RX = this
		override def + :RX = this
		override def *(n :Int) :RX = this
		override def *(min :AtLeast) :RX = this
		override def *(min :Int, max :Int) :RX = this

		override def ?? :RX = this
		override def *? :RX = this
		override def +? :RX = this
		override def *?(n :Int) :RX = this
		override def *?(min :AtLeast) :RX = this
		override def *?(min :Int, max :Int) :RX = this

		override def ?+ :RX = this
		override def *+ :RX = this
		override def ++ :RX = this
		override def *+(n :Int) :RX = this
		override def *+(min :AtLeast) :RX = this
		override def *+(min :Int, max :Int) :RX = this

		override def appendTo(res :StringBuilder) :Unit = ()

		override def toString = ""
	}

	private class AdapterRX(override val toString :String, override val groups :Seq[String]) extends RX {
		def this(pattern :String) = this(pattern, Nil)

		override def appendTo(res :StringBuilder) :Unit = res append toString
	}



	private[RX] class AtomicRX(override val toString :String) extends RX {
		override def ncgroup :RX = this

		override def appendTo(res :StringBuilder) :Unit = res append toString
	}



	private[RX] class RXGroup(val prefix :String, body :RX) extends RX {
		def this(body :RX) = this(":", body)

		override def ncgroup :RX = this

		override def groups :Seq[String] = body.groups

		override def appendTo(res :StringBuilder) :Unit = {
			res append "(?" append prefix
			body appendTo res
			res append ")"
		}
	}

	private[RX] class NonCapturingGroup(body :RX) extends RXGroup(":", body) {
		override def ~> :RX = body.~>

		override def <~ :RX = body.<~

		override def !~> :RX = body.!~>

		override def <~! :RX = body.<~!

		override def possessive :RX = body.possessive

		override def apply(flags :Flag*) :RX = body(flags :_*)

		override def group(name :String) :RX =	body.group(name)
	}


	private[RX] class NamedGroup(name :String, body :RX) extends RXGroup(adapt("<" + name +'>')) {
		override def groups :Seq[String] = name +: body.groups
	}

	private[RX] class FlaggedGroup(body :RX, flags :Seq[Flag])
		extends RXGroup(flags.filter(_.isOn).mkString + flags.filterNot(_.isOn).mkString("-", "", ":"), body)



	private[RX] class RepeatedRX(body :RX, symbol :String) extends RX {
		override def groups :Seq[String] = body.groups

		override def appendTo(res :StringBuilder) :Unit = {
			body appendTo res
			res append symbol
		}
	}



	private[RX] class QuantifiedRX(body :RX, min :Int, max :Int, symbol :String = "") extends RX {
		override def groups :Seq[String] = body.groups

		override def appendTo(res :StringBuilder) :Unit = {
			body appendTo res
			res append '{'
			if (min == max)
				res append min
			else {
				res append min append ','
				if (max >= 0)
					res append max
			}
			res append '}' append symbol
		}
	}


	private[RX] class Alternative(first :RX, second :RX) extends RX {

		override def groups :Seq[String] = listGroups(Nil)

		private[matching] override def listGroups(acc :Seq[String]) :Seq[String] = first.listGroups(second.listGroups(acc))

		private[matching] override def appendTo(res :StringBuilder) :Unit = {
			first appendTo res
			res append '|'
			second appendTo res
		}
	}



	private[RX] class Concatenation(first :RX, second :RX) extends RX {

		override def groups :Seq[String] = listGroups(Nil)

		override def listGroups(acc :Seq[String]) :Seq[String] = first.listGroups(second listGroups acc)

		private[matching] override def appendTo(res :StringBuilder) :Unit = {
			first appendTo res
			second appendTo res
		}
	}





	/** A regular expression matching exactly one character, representable as a complex character class `[...]`.
	  * In addition to the standard [[net.noresttherein.slang.matching.RX RX]] methods, it provides a way
	  * for combining several instances in both conjunction (`[...&&[...]]`) and disjunction (`[a-z0-9]`),
	  * as well as negating any such class (`[^...]`).
	  * Instances can be obtained in several ways:
	  *   - by implicit conversion from `Char` (requires importing of `RX.CharLiteral`);
	  *   - by implicit converter from `Char`: `'x'.rx` (requires importing of `RX.CharToRX`);
	  *   - using string interpolation for unicode character classes:
	  *     `p"Alphabetic", script"Latin", blk"Greek", prop"Digit", x"7F', o"377"` (requires importing of `RX.RXInterpolation`).
	  * @see [[net.noresttherein.slang.matching.RX.CharLiteral CharLiteral]]
	  */
	abstract class CharacterClass extends RX {

		override def |(other :RX) :RX = other match {
			case char :CharacterClass => this | char
			case _ => new Alternative(this, other)
		}

		def unary_! :CharacterClass = new NegatedClass(this)

		def &&(other :CharacterClass) :CharacterClass = new ConjunctionCharClass(this, other)

		def ||(other :CharacterClass) :CharacterClass = this | other

		def |(other :CharacterClass) :CharacterClass = this |: other

		private[RX] def |:(other :CharacterClass) :CharacterClass = new DisjunctionCharClass(other, this)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[CharacterClass]

		private[RX] def classBody :String

		private[matching] override def appendTo(res :StringBuilder) :Unit = {
			res += '['
			res ++= classBody
			res += ']'
		}

	}


	private def adapt(classBody :String) :CharacterClass = new AdapterCharacterClass(classBody)

	private class AdapterCharacterClass(override val classBody :String) extends CharacterClass


	/** A regular expression for a character class which is valid both inside the '[...]' brackets and as a standalone
	  * regular expression (such as posix character classes). This distinction exists to avoid unnecessarily cluttering
	  * the resulting pattern with '()' and '[]'.
	  * @see [[net.noresttherein.slang.matching.RX.CharLiteral CharLiteral]]
	  */
	private def atom(regexp :String) :CharacterClass = new AtomicCharClass(regexp)

	private class AtomicCharClass(override val classBody :String) extends CharacterClass {

		override def ncgroup :RX = this

		override def appendTo(res :StringBuilder) :Unit = res append classBody

		override def toString :String = classBody
	}



	implicit class CharToRX(private val char :Char) extends AnyVal {
		def rx = new CharLiteral(char)

		private[RX] def \ :RX = atom("\\" + char)
	}


	/** A regular expression matching exactly the given character. If `char` has special meaning in the expression
	  * engine (such as '.' or '$') it will be escaped where applicable. This is an implicit class, providing automatic
	  * conversion from `Char` values.
	  * @param char the literal character to match.
	  */
	implicit class CharLiteral(private val char :Char) extends CharacterClass {

		def -(to :Char) :CharacterClass = new CharacterRange(char, to)

		def -(to :CharLiteral) :CharacterClass = new CharacterRange(char, to.char)

		override def ncgroup :RX = this


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[CharLiteral]


		override def classBody :String =
			if (char == '\\' || char == ']' || char == '[' || char == '-' || char == '^')
				"\\" + char
			else char.toString

		private[matching] override def appendTo(res :StringBuilder) :Unit = {
			if (("\\.?*+()[]{}$^" indexOf char) >= 0)
				res append '\\'
			res append char
		}


	}


	/** Implicit conversion from char bounds given as a `NumericRange[Char]` into a `CharacterClass` regular expression.
	  * A range `0 to 9` will be represented as `[0-9]`.
	  */
	implicit def characterRange(range :NumericRange.Inclusive[Char]) :CharacterClass =
		new CharacterRange(range.start, range.end)

	/** Implicit conversion from char bounds given as a `Char` pair into a `CharacterClass` regular expression.
	  * A tuple `0 -> 9` will be represented as `[0-9]`.
	  */
	implicit def characterRange(range :(Char, Char)) :CharacterClass =
		new CharacterRange(range._1, range._2)


	private class CharacterRange(start :Char, end :Char) extends CharacterClass {
		private[RX] override def classBody :String = start.toString + '-' + end
	}



	private class NegatedClass(body :CharacterClass) extends CharacterClass {
		override def unary_! :CharacterClass = body

		override def &&(other :CharacterClass) :CharacterClass = body | !other

		override def |(other :CharacterClass) :CharacterClass = new NegatedClass(body && !other)

		override def |:(other :CharacterClass) :CharacterClass = new NegatedClass(body && !other)


		private[RX] override def classBody :String = "^" + body
	}



	private class DisjunctionCharClass(first :CharacterClass, second :CharacterClass) extends CharacterClass {
		override def classBody :String = first.classBody + second.classBody
	}



	private class ConjunctionCharClass(first :CharacterClass, second :CharacterClass) extends CharacterClass {
		override def unary_! :CharacterClass = !(first | second)

		override def classBody :String = first.classBody + "&&[" + second.classBody + ']'
	}



	/** A type wrapping an `Int` signifying that a regular expression should be matched at least that number of times.
	  * New instances can be obtained either through the factory method `AtLeast(n)` or through the implicit extension
	  * [[net.noresttherein.slang.matching.RX.RepeatAtLeast]]: `1.-*`.
	  * @see [[net.noresttherein.slang.matching.RX.*(min:AtLeast)]]
	  */
	final class AtLeast(val n :Int) 

	/** Requests that a regular expression should be matched at least the given number of times. */
	def AtLeast(n :Int) :AtLeast = new AtLeast(n)

	/** Adds a `-*` method to an `Int`, producing an [[net.noresttherein.slang.matching.RX.AtLeast AtLeast]] instance. */
	implicit class RepeatAtLeast(private val n :Int) extends AnyVal {
		/** Requests that a regular expression should be matched at least the given number of times. */
		def -* :AtLeast = new AtLeast(n)
	}

}


