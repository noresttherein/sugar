package net.noresttherein.sugar.collection

import java.lang

import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.mutable.Builder
import scala.collection.{Factory, IterableOps, SpecificIterableFactory}
import scala.collection.immutable.ArraySeq.ofChar

import net.noresttherein.sugar.collection.ChunkedString.{AppendedString, Chunk, ConcatChunks, PrependedString}
import net.noresttherein.sugar.collection.Substring.SerializedSubstring



trait SpecificIterableOps[E, +CC[_], +C <: CC[E]] extends IterableOps[E, CC, C] {
	protected override def fromSpecific(coll :IterableOnce[E]) :C = specificFactory.fromSpecific(coll)
	protected override def newSpecificBuilder :Builder[E, C] = specificFactory.newBuilder
	override def empty :C = specificFactory.empty
	protected def specificFactory :SpecificIterableFactory[E, C]
}

trait StringLikeOps[+S <: Seq[Char]]
	extends CharSequence with SeqOps[Char, Seq, S] with SpecificIterableOps[Char, Seq, S]
{
	override def isEmpty :Boolean = length == 0
}

trait StringLike extends Seq[Char] with StringLikeOps[StringLike] with Serializable




/** A recursive sequence of strings with O(1) concatenation on both sides without a preference,
  * used as a temporary structure when building longer strings. Standard `toString` method recursively concatenates
  * all chunks, producing the full string represented by this instance. It is a `Seq[Char]`, and thus will equal any
  * other `Seq[Char]`, but not a `String`, even though equality is defined by its combined string representation
  * (ignoring the underlying structure, i.e. what chunks it concatenates), in order to remain reflective.
  * It is used when larger strings are built recursively from many parts in a complex pattern, and when the efficiency
  * of 'writing' is much more important than 'reading' - ''chunked'' strings are not expected to be accessed often
  * before their final conversion to a `String`.
  * Despite the lack of fast random access, `length`/`size` are still `O(1)` operations.
  */
sealed trait ChunkedString extends StringLike with StringLikeOps[ChunkedString] {
	override def knownSize :Int = length
	override def charAt(index :Int) :Char = apply(index)
	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)

	override def empty :ChunkedString = ChunkedString.empty
	override def isEmpty :Boolean = length == 0

	protected override def specificFactory :SpecificIterableFactory[Char, ChunkedString] = ChunkedString

	def +(char :Char) :ChunkedString = this ++ String.valueOf(char)

	def ++(string :String) :ChunkedString =
		if (string.length == 0) this
		else if (isEmpty) new Chunk(string)
		else new AppendedString(this, string)

	def ++(string :ChunkedString) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(this, string)

	def +:(char :Char) :ChunkedString = String.valueOf(char) ++: this

	def ++:(string :String) :ChunkedString =
		if (string.length == 0) this
		else if (isEmpty) new Chunk(string)
		else new PrependedString(string, this)

	def ++:(string :ChunkedString) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(string, this)

	def *(n :Int) :ChunkedString =
		if (n < 0)
			throw new IllegalArgumentException("Negative repeat count " + n + " for " + this + ".")
		else {
			var mask = Integer.highestOneBit(n)
			var res = ChunkedString.empty
			while (mask != 0) {
				res ++= res
				if ((n & mask) != 0)
					res ++= this
				mask >>= 1
			}
			res
		}

	private def writeReplace :ChunkedString = new Chunk(toString)

	override def className :String = "ChunkedString"

	protected def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder

	override def toString :String = appendTo(new java.lang.StringBuilder).toString
}






object LowPriorityChunkedStringImplicits {
	implicit def chunkedStringChunkedFactory(companion :ChunkedString.type) :Factory[String, ChunkedString] =
		companion.factory
}


/** Brings into the implicit scope of the object `ChunkedString` an implicit conversion in the companion object
  * from the `ChunkedString` singleton object itself to standard Scala `Factory[Char, ChunkedString]`
  * or `Factory[String, ChunkedString]`. This allows use of the former as an argument for standard method `to`:
  * {{{
  *     val seq = Seq("You", "Boo", "I")
  *     val str = seq to ChunkedString
  * }}}
  */
sealed abstract class LowPriorityChunkedStringImplicits


object ChunkedString extends LowPriorityChunkedStringImplicits with SpecificIterableFactory[Char, ChunkedString] {
	private final val ver = 1L
	override val empty :ChunkedString = Empty

	private val emptyChunk = new Chunk("")


	def apply() :ChunkedString = empty

	def apply(string :String, strings :String*) :ChunkedString =
		string ++: ChunkedString(strings)

	def apply(string :String) :ChunkedString =
		if (string.length == 0) empty else new Chunk(string)

	def apply(strings :Seq[String]) :ChunkedString =
		if (strings.isEmpty) empty
		else if (strings.sizeIs == 1) ChunkedString(strings.head)
		else (chunkedBuilder ++= strings).result()

	@inline def from(chars :IterableOnce[Char]) :ChunkedString = fromSpecific(chars)

	override def fromSpecific(it :IterableOnce[Char]) :ChunkedString = it match {
		case chunks :ChunkedString => chunks
		case it :Iterable[Char] if it.isEmpty => Empty
		case it :WrappedString => new Chunk(it.unwrap)
//		case it :Iterable[Char] => //don't box!
//			val res =
//				if (it.knownSize > 0) new java.lang.StringBuilder(it.knownSize)
//				else new lang.StringBuilder
//			it foreach res.append
//			res.toString
		case seq :ofChar =>
			val a = seq.unsafeArray
			var i = 0; val len = a.length
			val res = new lang.StringBuilder(len)
			while (i < len) {
				res append a(i)
				i += 1
			}
			ChunkedString(res.toString)
		case it =>
			val i = it.iterator
			if (i.hasNext) {
				val res =
					if (it.knownSize > 0) new java.lang.StringBuilder(it.knownSize)
					else new lang.StringBuilder
				i foreach res.append
				new Chunk(res.toString)
			} else
				Empty
	}


	implicit def fromString(s :String) :ChunkedString = new Chunk(s)

	override def newBuilder :Builder[Char, ChunkedString] = new ChunkStringBuilder

	def chunkedBuilder :Builder[String, ChunkedString] =  new Builder[String, ChunkedString] {
		private [this] var chunks :ChunkedString = ChunkedString.empty

		override def addOne(elem :String) = { chunks ++= elem; this }

		override def result() = { val res = chunks; chunks = ChunkedString.empty; res }

		override def clear() :Unit = chunks = ChunkedString.empty
	}

	def factory :Factory[String, ChunkedString] = chunkedFactoryInstance

	private[this] val chunkedFactoryInstance :Factory[String, ChunkedString] =
		new Factory[String, ChunkedString] {
			override def fromSpecific(it :IterableOnce[String]) = (newBuilder ++= it).result()
			override def newBuilder = chunkedBuilder
		}


	@SerialVersionUID(ver)
	private object Empty extends ChunkedString {
		override def apply(idx :Int) = throw new IndexOutOfBoundsException("Empty ChunkedString access.")
		override def length = 0
		override def iterator :Iterator[Char] = Iterator.empty[Char]

		protected override def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder = builder
		override def toString = ""
	}



	@SerialVersionUID(ver)
	private class Chunk(s :String)
		extends ChunkedString with IndexedSeq[Char] with IndexedSeqOps[Char, IndexedSeq, Chunk]
	{
		override def head :Char = s.charAt(0)
		override def last :Char = s.charAt(s.length - 1)
		override def apply(i :Int) = s.charAt(i)

		override def length = s.length
		override def isEmpty = s.length == 0

		override def empty = emptyChunk
		override def iterator = new StringOps(s).iterator

		override def newSpecificBuilder :Builder[Char, Chunk] =
			(new StringBuilder).mapResult(new Chunk(_))

		protected override def fromSpecific(coll :IterableOnce[Char]) :Chunk = coll match {
			case s :Chunk => s
			case _ => new Chunk((new StringBuilder ++= coll).result())
		}

		protected override def appendTo(builder :java.lang.StringBuilder) = builder.append(s)

		override def toString = s
	}



	@SerialVersionUID(ver)
	private class AppendedString(prefix :ChunkedString, suffix :String) extends ChunkedString {
		override def head = if (prefix.isEmpty) suffix.charAt(0) else prefix.head
		override def last = suffix.charAt(suffix.length - 1)
		override def apply(i :Int) :Char = if (i < prefix.length) prefix(i) else suffix.charAt(i - prefix.length)

		override val length = prefix.length + suffix.length
		override def iterator = prefix.iterator ++ new StringOps(suffix).iterator

		override protected def appendTo(builder :java.lang.StringBuilder) =
			prefix appendTo builder append suffix

		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private class PrependedString(prefix :String, suffix :ChunkedString) extends ChunkedString {
		override def head = prefix.charAt(0)
		override def last = if (suffix.isEmpty) prefix.charAt(prefix.length - 1) else suffix.last
		override def apply(idx :Int) :Char = if (idx < prefix.length) prefix.charAt(idx) else suffix(idx - prefix.length)

		override val length = prefix.length + suffix.length
		override def iterator = new StringOps(prefix).iterator ++ suffix.iterator

		override protected def appendTo(builder :java.lang.StringBuilder) =
			suffix appendTo (builder append prefix)

		override def equals(that :Any) :Boolean = super.equals(that)
		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private class ConcatChunks(prefix :ChunkedString, suffix :ChunkedString) extends ChunkedString {
		override def head = if (prefix.isEmpty) suffix.head else prefix.head
		override def last = if (suffix.isEmpty) prefix.last else suffix.last
		override def apply(idx :Int) = if (idx < prefix.length) prefix(idx) else suffix(idx - prefix.length)

		override val length = prefix.length + suffix.length
		override def iterator = prefix.iterator ++ suffix

		override def appendTo(builder :java.lang.StringBuilder) =
			suffix.appendTo(prefix.appendTo(builder))

		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private final class ChunkStringBuilder extends Builder[Char, ChunkedString] {
		private[this] var chunkBuilder :java.lang.StringBuilder = _
		private[this] var chunks :ChunkedString = _
		private[this] var hinted :Boolean = false //if hinted then chunkBuilder != null && chunkBuilder.capacity == hint

		override def knownSize :Int =
			(if (chunkBuilder != null) chunkBuilder.length else 0) + (if (chunks != null) chunks.length else 0)

		override def sizeHint(size :Int) :Unit =
			if (size > 0 && chunkBuilder == null && (chunks == null || size > chunks.length)) {
				chunkBuilder = new java.lang.StringBuilder(if (chunks == null) size else size - chunks.length)
				hinted = true
			}

		override def addOne(elem :Char) :this.type = {
			if (chunkBuilder == null)
				chunkBuilder = new lang.StringBuilder
			chunkBuilder append elem
			this
		}

		override def addAll(xs :IterableOnce[Char]) :this.type = xs match {
			case it :Iterable[Char] if it.isEmpty =>
				this
			case chunk :ChunkedString =>
				flush()
				if (chunks == null) chunks = chunk
				else chunks ++= chunk
				this
			case str :WrappedString if !hinted || str.length > chunkBuilder.capacity - chunkBuilder.length =>
				flush()
				if (chunks == null)
					chunks = new Chunk(str.unwrap)
				else
					chunks ++= str.unwrap
				this
			case str :WrappedString =>
				chunkBuilder append str.unwrap; this
			case seq :ofChar if !hinted || seq.length > chunkBuilder.capacity - chunkBuilder.length =>
				flush()
				if (chunks == null)
					chunks = new Chunk(String.valueOf(seq.unsafeArray))
				else
					chunks ++= String.valueOf(seq.unsafeArray)
				this
			case seq :ofChar        => addAll(seq.unsafeArray); this
			case it :Iterable[Char] => addAll(it); this
			case it                 => addAll(it.iterator); this //higher chance of JIT kicking in when extracted
		}

		private[this] def addAll(it :Iterable[Char]) :Unit = {
			val size_? = it.knownSize
			if (chunkBuilder == null)
				chunkBuilder =
					if (size_? > 0) new java.lang.StringBuilder(size_?)
					else new java.lang.StringBuilder
			else if (size_? > 0)
				chunkBuilder ensureCapacity chunkBuilder.length + size_?
			it foreach chunkBuilder.append //boxing in every call
		}

		private[this] def addAll(chars :Array[Char]) :Unit = {
			val len = chars.length
			if (len > 0) {
				if (chunkBuilder == null)
					chunkBuilder = new java.lang.StringBuilder(len)
				else
					chunkBuilder ensureCapacity chunkBuilder.length + len
				chunkBuilder append chars
			}
		}

		private[this] def addAll(it :Iterator[Char]) :Unit =
			if (it.hasNext) {
				val size_? = it.knownSize
				if (chunkBuilder == null)
					chunkBuilder =
						if (size_? > 0) new java.lang.StringBuilder(size_?)
						else new java.lang.StringBuilder
				else if (size_? > 0)
					chunkBuilder ensureCapacity chunkBuilder.length + size_?
				while (it.hasNext)
					chunkBuilder append it.next()
			}

		override def result() :ChunkedString = {
			if (chunks == null)
				if (chunkBuilder != null && chunkBuilder.length > 0)
					chunks = new Chunk(chunkBuilder.toString)
				else
					chunks = Empty
			else if (chunkBuilder != null && chunkBuilder.length > 0)
				chunks ++= chunkBuilder.toString
			val res = chunks
			clear()
			res
		}

		private def flush() :Unit = {
			hinted = false
			if (chunkBuilder != null && chunkBuilder.length > 0) {
				if (chunks == null) chunks = new Chunk(chunkBuilder.toString)
				else chunks ++= chunkBuilder.toString
				chunkBuilder = null
			}
		}

		override def clear() :Unit = { chunkBuilder = null; chunks = null; hinted = false }
	}

}










/** A view of a section of a `String`. Because [[String.substring]] copies the contents, it is not suitable
  * for implementing algorithms which recursively divide the input sequence. All slicing operation (and indexed access)
  * are `O(1)`, at the cost of retaining a reference to the whole original string. For this reason this class
  * should be used as an intermediate data type, rather than a [[Seq]] implementation
  * passed to or from uncontrolled code. As an [[IndexedSeq]], it will equal any collection containing the same `Char`s.
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
final class Substring private (string :String, offset :Int, override val length :Int)
	extends AbstractSeq[Char] with IndexedSeq[Char] with ChunkedString
	   with StringLikeOps[Substring] with SeqOps[Char, IndexedSeq, Substring]
{
	protected override def specificFactory :SpecificIterableFactory[Char, Substring] = Substring

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

	override def reverse :Substring = {
		val sb = new java.lang.StringBuilder(length)
		var i = length
		while (i > offset) {
			i -= 1
			sb append string.charAt(i)
		}
		new Substring(sb.toString, 0, length)
	}

	override def empty :Substring = Substring.empty

	override def iterator :StringIterator = new StringIterator(string, offset, offset + length)

	override def reverseIterator :ReverseStringIterator = new ReverseStringIterator(string, offset, offset + length)

	override protected def appendTo(builder :lang.StringBuilder) :lang.StringBuilder = {
		var i = offset; val end = offset + length
		while (i < end) {
			builder append string.charAt(i)
			i += 1
		}
		builder
	}

	override def toString :String = string.substring(offset, offset + length)

	private def writeReplace :SerializedSubstring =
		if (length == 0) Substring.emptySerialized
		else new SerializedSubstring(string.substring(offset, offset + length))
}



/** A factory of views on substrings of a `String`. */
object Substring extends SpecificIterableFactory[Char, Substring] {
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

	override def newBuilder :Builder[Char, Substring] = new StringBuilder().mapResult(apply(_))

	override def fromSpecific(it :IterableOnce[Char]) :Substring = it match {
		case substring :Substring => substring
		case it :Iterable[Char] if it.isEmpty => empty
		case _ => (newBuilder ++= it).result()
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

