package net.noresttherein.sugar.collection

import scala.collection.immutable.AbstractSeq

import net.noresttherein.sugar.collection.Substring.SerializedSubstring




/** A view of a section of a `String`. Because [[String.substring]] copies the contents, it is not suitable
  * for implementing algorithms which recursively divide the input sequence. All slicing operation (and indexed access)
  * are `O(1)`, at the cost of retaining a reference to the whole original string. For this reason this class
  * should be used as an intermediate data type, rather than a [[Seq]] implementation
  * passed to or from uncontrolled code. As an [[IndexedSeq]], it will equal any collection containing the same `Char`s.
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
final class Substring private (string :String, offset :Int, override val length :Int)
	extends AbstractSeq[Char] with IndexedSeq[Char] with CharSequence
{
	override def isEmpty :Boolean = length == 0

	override def apply(i :Int) :Char =
		if (i < 0 || i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			string.charAt(offset + length)

	override def charAt(index :Int) :Char = apply(index)

	override def head :Char =
		if (length == 0) throw new NoSuchElementException("head of an empty Substring")
		else string.charAt(offset)

	override def tail :Substring =
		if (length == 0) throw new UnsupportedOperationException("tail of an empty Substring")
		else new Substring(string, offset + 1, length - 1)

	override def last :Char =
		if (length == 0) throw new NoSuchElementException("last of an empty Substring")
		else string.charAt(offset + length - 1)

	override def init :Substring =
		if (length == 0) throw new UnsupportedOperationException("init of an empty Substring")
		else new Substring(string, offset, length - 1)

	override def take(n :Int) :Substring =
		if (n <= 0) empty
		else if (n >= length) this
		else new Substring(string, offset, n)

	override def drop(n :Int) :Substring =
		if (n <= 0) this
		else if (n >= length) empty
		else new Substring(string, offset + n, length - n)

	override def takeRight(n :Int) :Substring =
		if (n <= 0) empty
		else if (n >= length) this
		else new Substring(string, offset + length - n, n)

	override def dropRight(n :Int) :Substring =
		if (n <= 0) this
		else if (n >= length) empty
		else new Substring(string, offset, length - n)

	override def slice(from :Int, until :Int) :Substring =
		if (until <= 0 || from >= length || until <= from) empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) new Substring(string, offset, until)
		else if (until >= length) new Substring(string, offset + from, length - from)
		else new Substring(string, offset + from, until - from)

	override def subSequence(start :Int, end :Int) :Substring = slice(start, end)

	def substring(start :Int, end :Int = length) :Substring = slice(start, end)

	override def splitAt(n :Int) :(Substring, Substring) =
		if (n <= 0) (empty, this)
		else if (n >= length) (this, empty)
		else (new Substring(string, offset, n), new Substring(string, offset + n, length - n))

	override def empty :Substring = Substring.empty

	override def iterator :StringIterator = new StringIterator(string, offset, offset + length)

	override def reverseIterator :ReverseStringIterator = new ReverseStringIterator(string, offset, offset + length)

	override def toString :String = string.substring(offset, offset + length)

	private def writeReplace :SerializedSubstring =
		if (length == 0) Substring.emptySerialized
		else new SerializedSubstring(string.substring(offset, offset + length))
}



/** A factory of views on substrings of a `String`. */
object Substring {
	def apply(string :String) :Substring = new Substring(string, 0, string.length)

	def apply(string :String, offset :Int) :Substring =
		if (offset >= string.length) empty
		else if (offset <= 0) new Substring(string, 0, string.length)
		else new Substring(string, offset, string.length - offset)

	def apply(string :String, from :Int, until :Int) :Substring = {
		val len = string.length
		if (from >= len || until <= 0) empty
		else if (from <= 0 && until >= len) new Substring(string, 0, len)
		else if (from <= 0) new Substring(string, 0, until)
		else if (until >= len) new Substring(string, from, len - from)
		else new Substring(string, from, until - from)
	}


	val empty :Substring = new Substring("", 0, 0)

	@SerialVersionUID(1L)
	private class SerializedSubstring(string :String) extends Serializable {
		private def readResolve = new Substring(string, 0, string.length)
	}
	private val emptySerialized = new SerializedSubstring("")
}






/** An iterator over an arbitrary section of a `String`,
  * similar to [[net.noresttherein.sugar.collection.ArrayIterator ArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class StringIterator private[collection] (string :String, from :Int, until :Int)
	extends scala.collection.BufferedIterator[Char]
{
	private[this] var index = from
	private[this] var stop = until

	override def hasNext :Boolean = index < stop

	override def head :Char = string.charAt(index)

	override def next() :Char = { val i = index; index += 1; string.charAt(i) }

	override def knownSize :Int = stop - index
	@inline override def size :Int = stop - index

	override def take(n :Int) :StringIterator =
		if (n >= size) this
		else if (n <= 0) { stop = index; this }
		else { stop = index + n; this }

	override def drop(n :Int) :StringIterator =
		if (n <= 0) this
		else if (n >= size) { index = stop; this } //this check is for Int overflow mostly
		else { index += n; this }

	override def slice(from :Int, until :Int) :StringIterator = take(until).drop(from)

	override def splitAt(n :Int) :(StringIterator, StringIterator) =
		if (n <= 0) (new StringIterator(string, index, index), this)
		else if (n >= length) (this, new StringIterator(string, stop, stop))
		else (new StringIterator(string, index, index + n), new StringIterator(string, index + n, stop))

}



object StringIterator {
	def apply(string :String) :StringIterator = new StringIterator(string, 0, string.length)

	def apply(string :String, offset :Int) :StringIterator =
		if (offset >= string.length) empty
		else if (offset <= 0) new StringIterator(string, 0, string.length)
		else new StringIterator(string, offset, string.length)

	def apply(string :String, from :Int, until :Int) :StringIterator = {
		val len = string.length
		if (from >= len || until <= 0 || from >= until) empty
		else if (from <= 0 && until >= len) new StringIterator(string, 0, len)
		else if (from <= 0) new StringIterator(string, 0, until)
		else if (until >= len) new StringIterator(string, from, len)
		else new StringIterator(string, from, until)
	}

	def empty = new StringIterator("", 0, 0)
}






/** An iterator over an arbitrary section of a `String`, running in reverse
  * similar to [[net.noresttherein.sugar.collection.ReverseArrayIterator ReverseArrayIterator]].
  * Has O(1) `take`/`drop`/`slice` methods.
  */
final class ReverseStringIterator private[collection] (string :String, from :Int, until :Int)
	extends scala.collection.BufferedIterator[Char]
{
	/* Requires 0 <= from <= until <= string.length and maintains invariant 0 <= stop <= index <= string.length.
	 * The invariant can be broken only by advancing an empty iterator.
	 * `string(until)` is the character immediately following (in string) the first character in this iterator,
	 * while `string(from)` is the last character in this iterator, unless it is empty.
	 * This scheme results in code being a mirror image of StringIterator, with + replaced with -.
	 */
	private[this] var index = until
	private[this] var stop = from

	override def hasNext :Boolean = index > stop

	override def head :Char = string.charAt(index - 1)

	override def next() :Char = { index -= 1; string.charAt(index) }

	override def knownSize :Int = index - stop
	@inline override def size :Int = index - stop

	override def take(n :Int) :ReverseStringIterator =
		if (n >= size) this
		else if (n <= 0) { stop = index; this }
		else { stop = index - n; this }

	override def drop(n :Int) :ReverseStringIterator =
		if (n <= 0) this
		else if (n >= size) { index = stop; this } //this check is for Int overflow mostly
		else { index -= n; this }

	override def slice(from :Int, until :Int) :ReverseStringIterator = take(until).drop(from)

	override def splitAt(n :Int) :(ReverseStringIterator, ReverseStringIterator) =
		if (n <= 0) (new ReverseStringIterator(string, index, index), this)
		else if (n >= length) (this, new ReverseStringIterator(string, stop, stop))
		else (new ReverseStringIterator(string, index, index - n), new ReverseStringIterator(string, index - n, stop))

}



object ReverseStringIterator {
	def apply(string :String) :ReverseStringIterator = new ReverseStringIterator(string, 0, string.length)

	def apply(string :String, from :Int, downto :Int) :ReverseStringIterator = {
		val len = string.length
		if (downto >= len || from <= 0 || downto >= from) empty
		else if (downto <= 0 && from >= len) new ReverseStringIterator(string, 0, len)
		else if (downto <= 0) new ReverseStringIterator(string, 0, from)
		else if (from >= len) new ReverseStringIterator(string, downto, len)
		else new ReverseStringIterator(string, downto, from)
	}

	def empty = new ReverseStringIterator("", 0, 0)
}
