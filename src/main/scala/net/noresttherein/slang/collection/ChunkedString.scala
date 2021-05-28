package net.noresttherein.slang.collection

import scala.collection.immutable.{IndexedSeqOps, SeqOps, StringOps}
import scala.collection.mutable.Builder

import net.noresttherein.slang.collection.ChunkedString.{AppendedString, Chunk, ConcatChunks, PrependedString}




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
	override def knownSize :Int= length
	override def charAt(index :Int) :Char = apply(index)
	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)

	override def empty :ChunkedString = ChunkedString.empty

	protected override def fromSpecific(coll :IterableOnce[Char]) :ChunkedString = coll match {
		case chunks :ChunkedString => chunks
		case _ => new Chunk(coll.toString)
	}

	protected override def newSpecificBuilder :Builder[Char, ChunkedString] =
		(new StringBuilder).mapResult(new Chunk(_))

	def +(char :Char) :ChunkedString = this + String.valueOf(char)

	def +(string :String) :ChunkedString =
		if (string.length == 0) this
		else if (isEmpty) ChunkedString(string)
		else new AppendedString(this, string)

	def +(string :ChunkedString) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(this, string)

	def +:(char :Char) :ChunkedString = String.valueOf(char) +: this

	def +:(string :String) :ChunkedString =
		if (string.isEmpty) this
		else if (isEmpty) ChunkedString(string)
		else new PrependedString(string, this)

	def +:(string :ChunkedString) :ChunkedString =
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
					res += this
				res += res
				mask >>= 1
			}
			res
		}

	override def className :String = "ChunkedString"

	protected def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder

	override def toString :String = appendTo(new java.lang.StringBuilder).toString
}




object ChunkedString {
	private final val ver = 1L
	val empty :ChunkedString = Empty

	private val emptyChunk = new Chunk("")

	def apply(string :String) :ChunkedString =
		if (string.length == 0) empty else new Chunk(string)

	implicit def fromString(s :String) :ChunkedString = new Chunk(s)



	@SerialVersionUID(ver)
	object Empty extends ChunkedString {
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

}

