package net.noresttherein.slang.collection

import java.lang

import scala.collection.immutable.{IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.mutable.Builder
import scala.collection.Factory

import net.noresttherein.slang.collection.ChunkedString.{AppendedString, Chunk, ChunkStringBuilder, ConcatChunks, PrependedString}




/** A recursive sequence of strings with O(1) concatenation on both sides without a preference,
  * used as a temporary structure when building longer strings. Standard `toString` method recursively concatenates
  * all chunks, producing the full string represented by this instance. It is a `Seq[Char]`, and thus will equal any
  * other `Seq[Char]`, but not a `String`, even though equality is defined by its combined string representation
  * (ignoring the underlying structure, i.e. what chunks it concatenates), in order to remain reflective.
  * It is used when larger strings are built recursively from many parts in a complex pattern, and when the efficiency
  * of 'writing' is much more important than 'reading' - ''chunked'' strings are not expected to be accessed often
  * before their final conversion to a `String`.
  */
sealed trait ChunkedString extends CharSequence with Seq[Char] with SeqOps[Char, Seq, ChunkedString] with Serializable {
	override def knownSize :Int = length
	override def charAt(index :Int) :Char = apply(index)
	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)

	override def empty :ChunkedString = ChunkedString.empty

	protected override def fromSpecific(coll :IterableOnce[Char]) :ChunkedString = coll match {
		case chunks :ChunkedString => chunks
		case _ => new Chunk(coll.toString)
	}

	protected override def newSpecificBuilder :Builder[Char, ChunkedString] =
		new ChunkStringBuilder

	def +(char :Char) :ChunkedString = this ++ String.valueOf(char)

	def ++(string :String) :ChunkedString =
		if (string.length == 0) this
		else if (isEmpty) ChunkedString(string)
		else new AppendedString(this, string)

	def ++(string :ChunkedString) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(this, string)

	def +:(char :Char) :ChunkedString = String.valueOf(char) ++: this

	def ++:(string :String) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) ChunkedString(string)
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
				if ((n & mask) != 0)
					res ++= this
				res ++= res
				mask >>= 1
			}
			res
		}

	override def className :String = "ChunkedString"

	protected def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder

	override def toString :String = appendTo(new java.lang.StringBuilder).toString
}






object LowPriorityChunkedStringImplicits {
	implicit def chunkedStringCharFactory(companion :ChunkedString.type) :Factory[Char, ChunkedString] =
		companion.factory

	implicit def chunkedStringChunkedFactory(companion :ChunkedString.type) :Factory[String, ChunkedString] =
		companion.chunkedFactory
}


/** Brings into the implicit scope of the object `ChunkedString` an implicit conversion in the companion object
  * from the `ChunkedString` singleton object itself to standard Scala `Factory[Char, ChunkedString]`
  * or `Factory[String, ChunkedString]`. This allows use of the former as an argument for standard method `to`:
  * {{{
  *     val seq = Seq("You", "Boo", "I")
  *     val str = seq to ChunkedString
  * }}}
  */
class LowPriorityChunkedStringImplicits


object ChunkedString extends LowPriorityChunkedStringImplicits {
	private final val ver = 1L
	val empty :ChunkedString = Empty

	private val emptyChunk = new Chunk("")

	def apply(string :String) :ChunkedString =
		if (string.length == 0) empty else new Chunk(string)

	def from(chars :IterableOnce[Char]) :ChunkedString = chars match {
		case chunks :ChunkedString => chunks
		case it :Iterable[Char] if it.isEmpty => Empty
		case it :WrappedString => new Chunk(it.unwrap)
		case it :Iterable[Char] =>
			val res =
				if (it.knownSize > 0) new java.lang.StringBuilder(it.knownSize)
				else new lang.StringBuilder
			it foreach res.append
			res.toString
		case it =>
			val i = it.iterator
			if (i.hasNext) {
				val res = new java.lang.StringBuilder
				i foreach res.append
				res.toString
			} else
				Empty
	}


	implicit def fromString(s :String) :ChunkedString = new Chunk(s)

	def newBuilder :Builder[Char, ChunkedString] = new ChunkStringBuilder

	def factory :Factory[Char, ChunkedString] =
		new Factory[Char, ChunkedString] {
			override def fromSpecific(it :IterableOnce[Char]) = ChunkedString.from(it)
			override def newBuilder = ChunkedString.newBuilder
		}

	def chunkedFactory :Factory[String, ChunkedString] =
		new Factory[String, ChunkedString] {
			override def fromSpecific(it :IterableOnce[String]) = (newBuilder ++= it).result()

			override def newBuilder = new Builder[String, ChunkedString] {
				var chunks :ChunkedString = ChunkedString.empty

				override def addOne(elem :String) = { chunks ++= elem; this }

				override def result() = { val res = chunks; chunks = ChunkedString.empty; res }

				override def clear() :Unit = chunks = ChunkedString.empty
			}
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
			prefix appendTo (builder append prefix)

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



	private final class ChunkStringBuilder extends Builder[Char, ChunkedString] {
		private[this] var chunkBuilder :java.lang.StringBuilder = _
		private[this] var chunks :ChunkedString = _
		private[this] var hinted :Boolean = false //if hinted then chunkBuilder != null && chunkBuilder.capacity == hint

		override def knownSize :Int =
			(if (chunkBuilder != null) chunkBuilder.length else 0) + (if (chunks != null) chunks.length else 0)

		override def sizeHint(size :Int) :Unit = {
			val extra = size - knownSize
			if (extra > 0 && chunkBuilder == null) {
				chunkBuilder = new lang.StringBuilder(extra)
				hinted = true
			}
		}

		override def addOne(elem :Char) :this.type = {
			if (chunkBuilder == null)
				chunkBuilder = new lang.StringBuilder
			chunkBuilder append elem
			this
		}

		override def addAll(xs :IterableOnce[Char]) :this.type = xs match {
			case it :Iterable[Char] if it.isEmpty => this
			case chunk :ChunkedString =>
				hinted = false
				if (chunkBuilder != null && chunkBuilder.length > 0) {
					chunks =
						if (chunks == null) new Chunk(chunkBuilder.toString)
						else chunks ++ chunkBuilder.toString
					chunkBuilder = null
				}
				if (chunks == null) chunks = chunk
				else chunks ++= chunk
				this
			case str :WrappedString if !hinted || str.length > chunkBuilder.capacity =>
				hinted = false
				if (chunkBuilder != null && chunkBuilder.length > 0) {
					chunks ++= chunkBuilder.toString
					chunkBuilder = null
				}
				if (chunks == null)
					chunks = new Chunk(str.unwrap)
				else
					chunks ++= str.unwrap
				this
			case str :WrappedString =>
				chunkBuilder append str.unwrap
				this
			case it :Iterable[Char] => addAll(it); this //higher chance of JIT kicking in when extracted
			case it                 => addAll(it.iterator); this
		}

		private[this] def addAll(it :Iterable[Char]) :Unit = {
			val size_? = it.knownSize
			if (chunkBuilder == null)
				chunkBuilder =
					if (size_? > 0) new java.lang.StringBuilder(size_?)
					else new java.lang.StringBuilder
			else if (size_? > 0)
				chunkBuilder ensureCapacity size_?
			it foreach chunkBuilder.append
		}

		private[this] def addAll(it :Iterator[Char]) :Unit =
			if (it.hasNext) {
				if (chunkBuilder == null)
					chunkBuilder = new java.lang.StringBuilder
				val i = it.iterator
				while (i.hasNext)
					chunkBuilder append i.next()
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

		override def clear() :Unit = { chunkBuilder = null; chunks = null; hinted = false }
	}

}

