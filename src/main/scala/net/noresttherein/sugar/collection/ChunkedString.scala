package net.noresttherein.sugar.collection

import java.lang

import scala.collection.immutable.{IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.mutable.Builder
import scala.collection.{Factory, SpecificIterableFactory}
import scala.collection.immutable.ArraySeq.ofChar

import net.noresttherein.sugar.collection.ChunkedString.{AppendedString, Chunk, ChunkStringBuilder, ConcatChunks, PrependedString}




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
sealed trait ChunkedString extends CharSequence with Seq[Char] with SeqOps[Char, Seq, ChunkedString] with Serializable {
	override def knownSize :Int = length
	override def charAt(index :Int) :Char = apply(index)
	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)

	override def empty :ChunkedString = ChunkedString.empty
	override def isEmpty :Boolean = length == 0

	protected override def fromSpecific(coll :IterableOnce[Char]) :ChunkedString = coll match {
		case chunks :ChunkedString => chunks
		case _ => new Chunk(coll.toString)
	}

	protected override def newSpecificBuilder :Builder[Char, ChunkedString] =
		new ChunkStringBuilder

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

