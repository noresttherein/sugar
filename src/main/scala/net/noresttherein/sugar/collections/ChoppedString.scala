package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import java.io.{IOException, Reader, StringReader}
import java.lang

import scala.collection.{Factory, SpecificIterableFactory, Stepper, StepperShape}
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.immutable.ArraySeq.ofChar
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.{JIntIterator, JIterator, JStringBuilder}
import net.noresttherein.sugar.collections.ChoppedString.{AppendedString, ChoppedStringReader, ConcatChunks, PrependedString}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions.{JavaIteratorExtension, StepperExtension}




/** A universal interface for concatenable `String`-like collections.
  * @see [[net.noresttherein.sugar.collections.StringLike]]
  */
trait StringLikeOps[+S <: Seq[Char]]
	extends CharSequence with SeqOps[Char, Seq, S] with SpecificIterableOps[Char, Seq, S]
{
	override def isEmpty   :Boolean = length == 0
	override def knownSize :Int = length
	def intIterator :JIntIterator
	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
		intIterator.asInstanceOf[I]

	override def charAt(index :Int) :Char = apply(index)

	override def take(n :Int) :S = slice(0, n)
	override def drop(n :Int) :S = slice(n, length)
	override def takeRight(n :Int) :S = if (n <= 0) empty else slice(length - n, length)
	override def dropRight(n :Int) :S = if (n <= 0) coll else slice(0, length - n)
	override def slice(from :Int, until :Int) :S =
		if (until <= from | until <= 0 || from >= length) empty
		else if (from <= 0 & until >= length) coll
		else if (from <= 0) trustedSlice(0, until)
		else if (until >= length) trustedSlice(from, length)
		else trustedSlice(from, until)

	override def splitAt(n :Int) :(S, S) =
		if (n <= 0) (empty, coll)
		else if (n >= length) (coll, empty)
		else (trustedSlice(0, n), trustedSlice(n, length))

	protected def trustedSlice(from :Int, until :Int) :S
	private[collections] final def `->trustedSlice`(from :Int, until :Int) :S = trustedSlice(from, until)

	def +(char :Char) :StringLike
	def +(string :String) :StringLike
	def +(string :StringLike) :StringLike = this + string.toString


	def toReader :Reader

}


/** An interface for concatenable `String` like collections, used either as an intermediate buffer
  * when building a `String` or providing additional features over `String`. Note that it will not equal a `String`.
  */
trait StringLike extends Seq[Char] with SugaredIterable[Char] with StringLikeOps[StringLike] with Serializable {
	override def subSequence(start :Int, end :Int) :CharSequence =
		if (start < 0 | end < 0 | start > end || end > length)
			throw new IndexOutOfBoundsException("[" + start + ".." + end + ") out of " + length)
		else
			trustedSlice(start, end)
}




/** A recursive sequence of strings with O(1) concatenation on both sides without a preference,
  * used as a temporary structure when building longer strings. Standard `toString` method recursively concatenates
  * all chunks, producing the full string represented by this instance. It is a `Seq[Char]`, and thus will equal any
  * other `Seq[Char]`, but not a `String`, even though equality is defined by its combined string representation
  * (ignoring the underlying structure, i.e. what chunks it concatenates), in order to remain reflective.
  * It is used when larger strings are built recursively from many parts in a complex pattern, and when the efficiency
  * of writing is much more important than reading - ''chopped'' strings are not expected to be accessed often
  * before their final conversion to a `String`. Note that most traversing methods are implemented recursively
  * and may thus cause a [[StackOverflowError]] for very long strings; this class is intended for use cases such
  * implementing `toString` methods. Despite the lack of fast random access, `length`/`size` are still `O(1)` operations
  * as long as appended/prepended sequences' `length` is O(n), because their length is computed on concatenation.
  *
  * This class is very similar to generic [[net.noresttherein.sugar.collections.ZigZag ZigZag]],
  * but exposes API dedicated to concatenating `String`s and some other methods specific to `Char`,
  * as covariance of `Seq` forces boxing of elements. It differs from a [[scala.collection.StringView StringView]]
  * in that all mapping operations are strict.
  * @define Coll `ChoppedString`
  * @define coll chopped string
  */
sealed trait ChoppedString extends StringLike with StringLikeOps[ChoppedString] {
//	protected def depth :Int //we can create an efficient stack-based iterator and foreach/map/flatMap
//	override def empty :ChoppedString = ChoppedString.empty

	override def specificFactory :SpecificIterableFactory[Char, ChoppedString] = ChoppedString

	override def +(char :Char) :ChoppedString = this ++ String.valueOf(char)
	override def +(string :String) :ChoppedString = this ++ string
	override def +(string :StringLike) :ChoppedString = string match {
		case chunk :ChoppedString => this ++ chunk
		case _ if string.length == 0 => this
		case _ if length == 0 => specificFactory.fromSpecific(string)
		case _ => (newSpecificBuilder ++= this ++= string).result()
	}

	def ++(string :String) :ChoppedString =
		if (string.length == 0) this
		else if (isEmpty) Substring(string)
		else new AppendedString(this, string)

	def ++(string :ChoppedString) :ChoppedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(this, string)

	def +:(char :Char) :ChoppedString = String.valueOf(char) ++: this

	def ++:(string :String) :ChoppedString =
		if (string.length == 0) this
		else if (isEmpty) Substring(string)
		else new PrependedString(string, this)

	def ++:(string :ChoppedString) :ChoppedString =
		if (string.isEmpty) this
		else if (isEmpty) string
		else new ConcatChunks(string, this)

	def *(n :Int) :ChoppedString =
		if (n < 0)
			throw new IllegalArgumentException("Negative repeat count " + n + " for " + this + ".")
		else {
			var mask = Integer.highestOneBit(n)
			var res = ChoppedString.empty
			while (mask != 0) {
				res ++= res
				if ((n & mask) != 0)
					res ++= this
				mask >>= 1
			}
			res
		}

	override def apply(i :Int) :Char //overriden for specialization

	def isWhitespace :Boolean = {
		val i = jiterator
		var isWhite = true
		while (i.hasNext && { isWhite = Character.isWhitespace(i.next().toChar); isWhite })
			{}
		isWhite
	}
	def isAlphanumeric :Boolean = {
		val i = jiterator
		var isAlphanum = true
		while (i.hasNext && { isAlphanum = Character.isLetterOrDigit(i.next()); isAlphanum })
			{}
		isAlphanum
	}

	override def className :String = "ChoppedString"

	protected def appendTo(builder :JStringBuilder) :JStringBuilder

	override def toReader :Reader = new ChoppedStringReader(this)

	override def toString :String = appendTo(new JStringBuilder).toString
}


/** Definition of a second choice implicit conversion
  * of the [[net.noresttherein.sugar.collections.ChoppedString$ ChoppedString]] object
  * to a Scala [[scala.collection.Factory Factory]] appending whole `String`s (rather than individual `Char`s).
  * Included in the implicit search because object `ChoppedString` extends its companion trait.
  */
object LowPriorityChoppedStringImplicits {
	implicit def choppedStringChunkFactory(companion :ChoppedString.type) :Factory[String, ChoppedString] =
		companion.factory
}


/** Brings into the implicit scope of the object `ChoppedString` an implicit conversion in the companion object
  * from the `ChoppedString` singleton object itself to standard Scala `Factory[Char, ChoppedString]`
  * or `Factory[String, ChoppedString]`. This allows use of the former as an argument for standard method `to`:
  * {{{
  *     val seq = Seq("You", "Boo", "I")
  *     val str = seq to ChoppedString
  * }}}
  */
private[collections] sealed abstract class LowPriorityChoppedStringImplicits


/**
  * $factoryInfo
  * @define Coll `ChoppedString`
  * @define coll chopped string
  */
object ChoppedString extends LowPriorityChoppedStringImplicits with SpecificIterableFactory[Char, ChoppedString] {
	private final val Ver = 1L
	override val empty :ChoppedString = Empty

//	private val emptyChunk = new Chunk("")


	def apply() :ChoppedString = empty

	def apply(first :String, second :String, strings :String*) :ChoppedString =
		first ++: second ++: ChoppedString(strings)

	def apply(string :String) :ChoppedString =
		if (string.length == 0) empty else Substring(string)

	def apply(strings :Seq[String]) :ChoppedString =
		if (strings.isEmpty) empty
		else if (strings.sizeIs == 1) ChoppedString(strings.head)
		else (chunkedBuilder ++= strings).result()

	@inline def from(chars :IterableOnce[Char]) :ChoppedString = fromSpecific(chars)

	override def fromSpecific(it :IterableOnce[Char]) :ChoppedString = it match {
		case chunks :ChoppedString => chunks
		case it :Iterable[Char] if it.isEmpty => Empty
		case it :WrappedString => Substring(it.unwrap)
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
			ChoppedString(res.toString)
		case it =>
			val i = it.iterator
			if (i.hasNext) {
				val res =
					if (it.knownSize > 0) new java.lang.StringBuilder(it.knownSize)
					else new lang.StringBuilder
				i foreach res.append
				Substring(res.toString)
			} else
				Empty
	}


	implicit def choppedStringFromString(s :String) :ChoppedString =
		if (s.length == 0) Empty else Substring(s)

	override def newBuilder :Builder[Char, ChoppedString] = new ChoppedStringBuilder

	def chunkedBuilder :Builder[String, ChoppedString] =  new Builder[String, ChoppedString] {
		private [this] var chunks :ChoppedString = ChoppedString.empty

		override def addOne(elem :String) = { chunks ++= elem; this }

		override def result() = { val res = chunks; chunks = ChoppedString.empty; res }

		override def clear() :Unit = chunks = ChoppedString.empty
	}

	def factory :Factory[String, ChoppedString] = chunkedFactoryInstance

	private[this] val chunkedFactoryInstance :Factory[String, ChoppedString] =
		new Factory[String, ChoppedString] {
			override def fromSpecific(it :IterableOnce[String]) = (newBuilder ++= it).result()
			override def newBuilder = chunkedBuilder
		}


	@SerialVersionUID(Ver)
	private object Empty extends ChoppedString {
		override def apply(idx :Int) = throw new IndexOutOfBoundsException("Empty ChoppedString access.")
		override def length = 0
		override def iterator :Iterator[Char] = Iterator.empty[Char]
		override def intIterator = JavaIterator.ofInt()
		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I = JavaIterator()
		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S = Stepper0()

		override def foreach[U](f :Char => U) :Unit = {}

		override def trustedSlice(from :Int, until :Int) :ChoppedString = this
		protected override def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder = builder
		override def toReader = Reader.nullReader()
		override def toString = ""
	}



/*
	@SerialVersionUID(Ver)
	private class Chunk(s :String)
		extends ChoppedString with IndexedSeq[Char] with IndexedSeqOps[Char, IndexedSeq, Chunk]
	{
		override def head :Char =
			if (s.length == 0) throw new NoSuchElementException("ChoppedString().head")
			else s.charAt(0)
		override def last :Char =
			if (s.length == 0) throw new NoSuchElementException("ChoppedString().last")
			else s.charAt(s.length - 1)
		override def apply(i :Int) = s.charAt(i)

		override def length = s.length
		override def isEmpty = s.length == 0

		override def trustedSlice(from :Int, until :Int) :ChoppedString = s.substring(from, until)

		override def empty    = emptyChunk
		override def iterator = new StringOps(s).iterator
		override def intIterator = JavaIterator.ofInt(s)

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
			JavaIterator.ofInt(s).asInstanceOf[I]

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) =
			new StringOps(s).stepper.asInstanceOf[S with EfficientSplit]

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

		override def toReader :Reader = new StringReader(s)
		override def toString = s
	}
*/



	@SerialVersionUID(Ver)
	private class AppendedString(prefix :ChoppedString, suffix :String) extends ChoppedString {
		assert(suffix.length > 0, "Appended an empty String to " + suffix + ".")
		override val length = prefix.length + suffix.length
		override def head = if (prefix.isEmpty) suffix.charAt(0) else prefix.head
		override def last = suffix.charAt(suffix.length - 1)
		override def apply(i :Int) :Char = if (i < prefix.length) prefix(i) else suffix.charAt(i - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChoppedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) prefix.`->trustedSlice`(from, until)
			else if (from >= prefixLength) Substring(suffix, from - prefixLength, until - prefixLength)
			else prefix.`->trustedSlice`(from, prefixLength) ++ Substring(suffix, 0, until - prefixLength)
		}

		override def iterator = prefix.iterator ++ new StringOps(suffix).iterator
		override def intIterator = prefix.intIterator ++ JavaIterator.ofInt(suffix)

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
			prefix.jiterator ++ JavaIterator.ofInt(suffix).asInstanceOf[I]

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S =
			prefix.stepper ++ new StringOps(suffix).stepper.asInstanceOf[S]

		override def foreach[U](f :Char => U) :Unit = {
			prefix.foreach(f)
			new StringOps(suffix).foreach(f)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) =
			prefix appendTo builder append suffix

		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}



	@SerialVersionUID(Ver)
	private class PrependedString(prefix :String, suffix :ChoppedString) extends ChoppedString {
		assert(prefix.length > 0, "Prepended an empty String to " + suffix + ".")
		override val length = prefix.length + suffix.length
		override def head = prefix.charAt(0)
		override def last = if (suffix.isEmpty) prefix.charAt(prefix.length - 1) else suffix.last
		override def apply(idx :Int) :Char = if (idx < prefix.length) prefix.charAt(idx) else suffix(idx - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChoppedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) Substring(prefix, from, until)
			else if (from >= prefixLength) suffix.`->trustedSlice`(from - prefixLength, until - prefixLength)
			else prefix.substring(from, prefixLength) ++: suffix.`->trustedSlice`(0, until - prefixLength)
		}

		override def iterator = new StringOps(prefix).iterator ++ suffix.iterator
		override def intIterator = JavaIterator.ofInt(prefix) ++ suffix.intIterator

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
			JavaIterator.ofInt(prefix).asInstanceOf[I] ++ suffix.jiterator

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S =
			new StringOps(prefix).stepper.asInstanceOf[S] ++ suffix.stepper

		override def foreach[U](f :Char => U) :Unit = {
			new StringOps(prefix).foreach(f)
			suffix.foreach(f)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) =
			suffix appendTo (builder append prefix)

		override def equals(that :Any) :Boolean = super.equals(that)
		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}



	@SerialVersionUID(Ver)
	private class ConcatChunks(prefix :ChoppedString, suffix :ChoppedString) extends ChoppedString {
		assert(prefix.nonEmpty && suffix.nonEmpty,
			"Concatenation with an empty String: " + prefix.length + " + " + suffix.length + "."
		)
		override val length = prefix.length + suffix.length
		override def head = if (prefix.isEmpty) suffix.head else prefix.head
		override def last = if (suffix.isEmpty) prefix.last else suffix.last
		override def apply(idx :Int) = if (idx < prefix.length) prefix(idx) else suffix(idx - prefix.length)

		override def trustedSlice(from :Int, until :Int) :ChoppedString = {
			val prefixLength = prefix.length
			if (until <= prefixLength) prefix.`->trustedSlice`(from, until)
			else if (from >= prefixLength) suffix.`->trustedSlice`(from - prefixLength, until - prefixLength)
			else prefix.`->trustedSlice`(from, prefixLength) ++ suffix.`->trustedSlice`(0, until - prefixLength)
		}

		override def iterator = prefix.iterator ++ suffix
		override def intIterator = prefix.intIterator ++ suffix.intIterator

		override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
			prefix.jiterator ++ suffix.jiterator

		override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S =
			prefix.stepper ++ suffix.stepper

		override def foreach[U](f :Char => U) :Unit = {
			prefix foreach f
			suffix foreach f
		}

		override def appendTo(builder :java.lang.StringBuilder) =
			suffix.appendTo(prefix.appendTo(builder))

		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}



	private final class ChoppedStringBuilder extends Builder[Char, ChoppedString] {
		private[this] var chunkBuilder :java.lang.StringBuilder = _
		private[this] var chunks :ChoppedString = _
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
			case chunk :ChoppedString =>
				flush()
				if (chunks == null) chunks = chunk
				else chunks ++= chunk
				this
			case str :WrappedString if !hinted || str.length > chunkBuilder.capacity - chunkBuilder.length =>
				flush()
				if (chunks == null)
					chunks = Substring(str.unwrap)
				else
					chunks ++= str.unwrap
				this
			case str :WrappedString =>
				chunkBuilder append str.unwrap; this
			case seq :ofChar if !hinted || seq.length > chunkBuilder.capacity - chunkBuilder.length =>
				flush()
				if (chunks == null)
					chunks = Substring(String.valueOf(seq.unsafeArray))
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

		override def result() :ChoppedString = {
			if (chunks == null)
				if (chunkBuilder != null && chunkBuilder.length > 0)
					chunks = Substring(chunkBuilder.toString)
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
				if (chunks == null) chunks = Substring(chunkBuilder.toString)
				else chunks ++= chunkBuilder.toString
				chunkBuilder = null
			}
		}

		override def clear() :Unit = { chunkBuilder = null; chunks = null; hinted = false }
	}


	private class ChoppedStringReader(private[this] var tail :ChoppedString) extends Reader {
		override def read :Int = {
			if (tail.isEmpty)
				throw new IOException("empty string")
			val (h, t) = tail.splitAt(1)
			tail = t
			h.head
		}
		override def read(cbuf :Array[Char], off :Int, len :Int) = {
			val res = tail.copyToArray(cbuf, off, len)
			tail = tail.drop(res)
			res
		}
		override def close() :Unit = ()
	}

	override def toString = "ChoppedString"
}






private[collections] abstract class AbstractSubstring[C <: StringLike with IndexedSeq[Char]]
                                                     (string :String, offset :Int, override val length :Int)
	extends AbstractSeq[Char] with IndexedSeq[Char] with IndexedSeqOps[Char, IndexedSeq, C]
	   with StringLike with StringLikeOps[C] with SpecificIterableOps[Char, IndexedSeq, C]
{ this :C =>
	private def data = string
	private def start = offset

	override def head :Char =
		if (length == 0) throw new NoSuchElementException("Substring().head")
		else string.charAt(offset)

	override def last :Char =
		if (length == 0) throw new NoSuchElementException("Substring().last")
		else string.charAt(offset + length - 1)

	override def apply(i :Int) :Char =
		if (i < 0 || i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			string.charAt(offset + i)

	override def iterator :StringIterator = new StringIterator(string, offset, offset + length)
	override def reverseIterator :ReverseStringIterator = new ReverseStringIterator(string, offset, offset + length)
	override def intIterator :JIntIterator = JavaIterator.ofInt(string, offset, offset + length)

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
		intIterator.asInstanceOf[I]

	override def foreach[U](f :Char => U) :Unit = {
		var i = offset;
		val end = i + length
		while (i < end) {
			f(string.charAt(i))
			i += 1
		}
	}

	override def toReader :Reader =
		if (length == string.length) new StringReader(string)
		else new SubstringReader(string, offset, length)

	override def equals(that :Any) :Boolean = that match {
		case other :AbstractSubstring[_] if other.canEqual(this) && canEqual(other) =>
			length == other.length && {
				val otherString = other.data
				var i = offset
				var j = other.start
				val end = i + length
				while (i < end && string.charAt(i) == otherString.charAt(j)) {
					i += 1; j += 1
				}
				i == end
			}
		case _ => super.equals(that)
	}

	override def toString :String = string.substring(offset, offset + length)
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
	extends AbstractSubstring[Substring](string, offset, length) with ChoppedString
{
	assert(offset >= 0 & offset <= string.length,
		"Offset out of bounds: " + string + ".substring(" + offset + "->" + length + ")."
	)
	assert(length <= string.length - offset,
		"Length out of bounds: " + string + ".substring(" + offset + "->" + length + ")."
	)
	override def specificFactory :SpecificIterableFactory[Char, Substring] = Substring

	private def data = string
	private def start = offset

	protected override def trustedSlice(from :Int, until :Int) :Substring =
		new Substring(string, offset + from, until - from)

	def substring(start :Int, end :Int = length) :Substring = slice(start, end)

	override def reverse :Substring = {
		val sb = new java.lang.StringBuilder(length)
		var i = length
		while (i > offset) {
			i -= 1
			sb append string.charAt(i)
		}
		new Substring(sb.toString, 0, length)
	}

	override def isWhitespace :Boolean = {
		var i = offset; val until = offset + length
		while (i < until && Character.isWhitespace(string.charAt(i)))
			i += 1
		i == until
	}

	override def ++(string :ChoppedString) :ChoppedString =
		if (isEmpty) string
		else if (string.isEmpty) this
		else string match {
			case Substring(data, from, until) if (this.string eq data) && from == offset + length =>
				new Substring(this.string, offset, length + until - from)
			case _ => super.++(string)
		}
	override def ++:(string :ChoppedString) :ChoppedString =
		if (isEmpty) string
		else if (string.isEmpty) this
		else string match {
			case Substring(data, from, until) if (this.string eq data) && until == offset =>
				new Substring(this.string, from, until - from + length)
			case _ => super.++(string)
		}

	protected override def appendTo(builder :lang.StringBuilder) :lang.StringBuilder = {
		var i = offset; val end = offset + length
		while (i < end) {
			builder append string.charAt(i)
			i += 1
		}
		builder
	}

	private def writeReplace = new Substring(string.substring(offset, offset + length), 0, length)
}



/** A factory of views on substrings of a `String`.
  * $factoryInfo
  * $Coll `Substring`
  * $coll substring
  */
@SerialVersionUID(Ver)
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

	/** If `chars` is a [[net.noresttherein.sugar.collections.Substring! Substring]], extracts its underlying string
	  * as well as the indices of the first character and the character following the last character.
	  */
	def unapply(chars :Seq[Char]) :Opt[(String, Int, Int)] = chars match {
		case string :Substring => Got((string.data, string.start, string.start + string.length))
		case _ => Lack
	}
	/** If `chars` is a [[net.noresttherein.sugar.collections.Substring! Substring]], extracts its underlying string
	  * as well as the indices of the first character and the character following the last character.
	  */
	def unapply(chars :CharSequence) :Opt[(String, Int, Int)] = chars match {
		case string :Substring => Got((string.data, string.start, string.start + string.length))
		case _ => Lack
	}
	/** If `chars` is a [[net.noresttherein.sugar.collections.Substring! Substring]], extracts its underlying string
	  * as well as the indices of the first character and the character following the last character.
	  */
	@inline def unapply(chars :StringLike) :Opt[(String, Int, Int)] = unapply(chars :CharSequence)


	override def newBuilder :Builder[Char, Substring] = new StringBuilder().mapResult(apply(_))

	override def fromSpecific(it :IterableOnce[Char]) :Substring = it match {
		case substring :Substring => substring
		case it :Iterable[Char] if it.isEmpty => empty
		case _ => (newBuilder ++= it).result()
	}

	override val empty :Substring = new Substring("", 0, 0) //we could do read resolve to have a single instance

	override def toString = "Substring"
}




private class SubstringReader(string :String, start :Int, private[this] var length :Int) extends Reader {
	private[this] val reader = new java.io.StringReader(string)
	reader.skip(start)

	override def read(cbuf :Array[Char], off :Int, len :Int) =
		if (length == 0)
			-1
		else {
			val count = reader.read(cbuf, off, math.min(len, length))
			length -= count
			count
		}
	override def read() =
		if (length == 0)
			-1
		else {
			length -= 1
			reader.read()
		}
	override def skip(n :Long) = reader.skip(math.min(length, n))
	override def markSupported() = reader.markSupported()
	override def mark(readAheadLimit :Int) :Unit = reader.mark(readAheadLimit)
	override def reset() :Unit = reader.reset()
	override def ready() = reader.ready()

	override def close() :Unit = reader.close()

}
