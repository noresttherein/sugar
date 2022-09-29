package net.noresttherein.sugar.collection

import java.lang

import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.mutable.Builder
import scala.collection.{Factory, IterableOps, SpecificIterableFactory}
import scala.collection.immutable.ArraySeq.ofChar

import net.noresttherein.sugar.collection.ChunkedString.{AppendedString, Chunk, ConcatChunks, Empty, PrependedString}
import net.noresttherein.sugar.collection.Substring.SerializedSubstring
import net.noresttherein.sugar.JavaTypes.JStringBuilder



trait SpecificIterableOps[E, +CC[_], +C <: CC[E]] extends IterableOps[E, CC, C] {
	protected override def fromSpecific(coll :IterableOnce[E]) :C = specificFactory.fromSpecific(coll)
	protected override def newSpecificBuilder :Builder[E, C] = specificFactory.newBuilder
	override def empty :C = specificFactory.empty
	protected def specificFactory :SpecificIterableFactory[E, C]
}

trait StringLikeOps[+S <: Seq[Char]]
	extends CharSequence with SeqOps[Char, Seq, S] with SpecificIterableOps[Char, Seq, S]
{
	override def isEmpty   :Boolean = length == 0
	override def knownSize :Int = length

	override def charAt(index :Int) :Char = apply(index)
}

trait StringLike extends Seq[Char] with StringLikeOps[StringLike] with Serializable {
	def +(char :Char) :StringLike
	def +(string :String) :StringLike
	def +(string :StringLike) :StringLike = this + string.toString
}




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
	override def empty :ChunkedString = ChunkedString.empty

	protected override def specificFactory :SpecificIterableFactory[Char, ChunkedString] = ChunkedString

	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)
	override def take(n :Int) :ChunkedString = slice(0, n)
	override def drop(n :Int) :ChunkedString = slice(n, length)
	override def takeRight(n :Int) :ChunkedString = if (n <= 0) Empty else slice(length - n, length)
	override def dropRight(n :Int) :ChunkedString = if (n <= 0) this else slice(0, length - n)
	override def slice(from :Int, until :Int) :ChunkedString =
		if (until < from | until <= 0 || from >= length) Empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) trustedSlice(0, until)
		else trustedSlice(from, length)

	protected[collection] def trustedSlice(from :Int, until :Int) :ChunkedString

	override def +(char :Char) :ChunkedString = this ++ String.valueOf(char)
	override def +(string :String) :ChunkedString = this ++ string
	override def +(string :StringLike) :ChunkedString = string match {
		case chunk :ChunkedString => this ++ chunk
		case _ if string.length == 0 => this
		case _ if length == 0 => specificFactory.fromSpecific(string)
		case _ => (newSpecificBuilder ++= this ++= string).result()
	}

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

	protected def appendTo(builder :JStringBuilder) :JStringBuilder

	override def toString :String = appendTo(new JStringBuilder).toString
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

	override def newBuilder :Builder[Char, ChunkedString] = new ChunkedStringBuilder

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

		override def foreach[U](f :Char => U) :Unit = {}

		override def trustedSlice(from :Int, until :Int) :ChunkedString = this

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

		override def trustedSlice(from :Int, until :Int) :ChunkedString = s.substring(from, until)

		override def empty = emptyChunk
		override def iterator = new StringOps(s).iterator

		override def foreach[U](f :Char => U) :Unit = {
			var i = 0; val len = s.length
			while (i < len) {
				f(s.charAt(i))
				i += 1
			}
		}

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
		override val length = prefix.length + suffix.length
		override def head = if (prefix.isEmpty) suffix.charAt(0) else prefix.head
		override def last = suffix.charAt(suffix.length - 1)
		override def apply(i :Int) :Char = if (i < prefix.length) prefix(i) else suffix.charAt(i - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChunkedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) prefix.trustedSlice(from, until)
			else if (from >= prefixLength) new Chunk(suffix.substring(from - prefixLength, until - prefixLength))
			else prefix.trustedSlice(from, prefixLength) ++ suffix.substring(0, until - prefixLength)
		}

		override def iterator = prefix.iterator ++ new StringOps(suffix).iterator

		override def foreach[U](f :Char => U) :Unit = {
			prefix.foreach(f)
			new StringOps(suffix).foreach(f)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) =
			prefix appendTo builder append suffix

		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private class PrependedString(prefix :String, suffix :ChunkedString) extends ChunkedString {
		override val length = prefix.length + suffix.length
		override def head = prefix.charAt(0)
		override def last = if (suffix.isEmpty) prefix.charAt(prefix.length - 1) else suffix.last
		override def apply(idx :Int) :Char = if (idx < prefix.length) prefix.charAt(idx) else suffix(idx - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChunkedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) new Chunk(prefix.substring(from, until))
			else if (from >= prefixLength) suffix.trustedSlice(from - prefixLength, until - prefixLength)
			else prefix.substring(from, prefixLength) ++: suffix.trustedSlice(0, until - prefixLength)
		}

		override def iterator = new StringOps(prefix).iterator ++ suffix.iterator
		override def foreach[U](f :Char => U) :Unit = {
			new StringOps(prefix).foreach(f)
			suffix.foreach(f)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) =
			suffix appendTo (builder append prefix)

		override def equals(that :Any) :Boolean = super.equals(that)
		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private class ConcatChunks(prefix :ChunkedString, suffix :ChunkedString) extends ChunkedString {
		override val length = prefix.length + suffix.length
		override def head = if (prefix.isEmpty) suffix.head else prefix.head
		override def last = if (suffix.isEmpty) prefix.last else suffix.last
		override def apply(idx :Int) = if (idx < prefix.length) prefix(idx) else suffix(idx - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChunkedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) prefix.trustedSlice(from, until)
			else if (from >= prefixLength) suffix.trustedSlice(from - prefixLength, until - prefixLength)
			else prefix.trustedSlice(from, prefixLength) ++ suffix.trustedSlice(0, until - prefixLength)
		}

		override def iterator = prefix.iterator ++ suffix
		override def foreach[U](f :Char => U) :Unit = {
			prefix foreach f
			suffix foreach f
		}

		override def appendTo(builder :java.lang.StringBuilder) =
			suffix.appendTo(prefix.appendTo(builder))

		override lazy val toString :String = super.toString
	}



	@SerialVersionUID(ver)
	private final class ChunkedStringBuilder extends Builder[Char, ChunkedString] {
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

	protected[collection] override def trustedSlice(from :Int, until :Int) :ChunkedString =
		new Substring(string, offset + from, until - from)

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

	override def foreach[U](f :Char => U) :Unit = {
		var i = offset; val end = i + length
		while (i < end) {
			f(string.charAt(i))
			i += 1
		}
	}

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
