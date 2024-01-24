package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.io.{IOException, Reader, StringReader}
import java.lang

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.StepperShape.{CharShape, IntShape}
import scala.collection.{Factory, IntStepper, SpecificIterableFactory, Stepper, StepperShape, mutable}
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, SeqOps, WrappedString}
import scala.collection.mutable.{Buffer, Builder, ReusableBuilder}

import net.noresttherein.sugar.JavaTypes.{JIntIterator, JIterator, JStringBuilder}
import net.noresttherein.sugar.arrays.{ArrayLike, ErasedArray}
import net.noresttherein.sugar.collections.ChoppedString.{AppendedString, ChoppedStringReader, Chops, ConcatChunks, Empty, PrependedString, stringOf}
import net.noresttherein.sugar.collections.extensions.{StepperExtension, StepperCompanionExtension}
import net.noresttherein.sugar.typist.casting.extensions.castingMethods
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits




/** A universal interface for concatenable `String`-like collections.
  * @see [[net.noresttherein.sugar.collections.StringLike]]
  */
trait StringLikeOps[+S <: Seq[Char]]
	extends CharSequence with SeqOps[Char, Seq, S] with SeqSlicingOps[Char, Seq, S]
	   with SpecificIterableFactoryDefaults[Char, Seq, S]
{
	override def isEmpty   :Boolean = length == 0
	override def knownSize :Int = length

	def intIterator :JIntIterator = stepper.javaIterator
//	override def javaIterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
//		intIterator.asInstanceOf[I]

	override def charAt(index :Int) :Char = apply(index)
	def updated(index :Int, elem :Char) :S

	def map(f :Char => Char) :S = {
		val res = newSpecificBuilder
		res sizeHint length
		(res /: this) { (b, char) => b += f(char) }.result()
	}
	def flatMap(f :Char => StringLike) :S = {
		val res = newSpecificBuilder
		(res /: this) { (b, char) => b ++= f(char) }.result()
	}

	@inline final def +(char :Char) :S = appended(char)
	@inline final def :+(char :Char) :S = appended(char)
	def appended(char :Char) :S

	@inline final def +:(char :Char) :S = prepended(char)
	def prepended(char :Char) :S

	@inline final def :++(string :String) :S = appendedAll(string)
	def appendedAll(string :String) :S

	@inline final def ++:(string :String) :S = prependedAll(string)
	def prependedAll(string :String) :S

	@inline final def :++(string :StringLike) :S = appendedAll(string)
	def appendedAll(string :StringLike) :S = appendedAll(if (string == null) "null" else string.toString)

	@inline final def ++:(string :StringLike) :S = prependedAll(string)
	def prependedAll(string :StringLike) :S = prependedAll(if (string == null) "null" else string.toString)

	@inline final def :++(other :IterableOnce[Char]) :S = appendedAll(other)
	def appendedAll(other :IterableOnce[Char]) :S

	@inline final def ++:(other :IterableOnce[Char]) :S = prependedAll(other)
	def prependedAll(other :IterableOnce[Char]) :S
	//consider: S as the return type
//	@inline def ++(string :String) :StringLike = concat(string)
//	@inline def ++(string :StringLike) :StringLike = concat(string)
	def concat(string :String) :StringLike
	def concat(string :StringLike) :StringLike

	def toReader :Reader
}


/** An interface for concatenable `String` like collections, used either as an intermediate buffer
  * when building a `String` or providing additional features over `String`. Note that it will not equal a `String`.
  */
trait StringLike extends Seq[Char] with SugaredIterable[Char] with StringLikeOps[StringLike] with Serializable {
	override def subSequence(start :Int, end :Int) :CharSequence =
		if (start < 0 | end < 0 | start > end | end > length)
			throw new IndexOutOfBoundsException("[" + start + ".." + end + ") out of " + length)
		else
			clippedSlice(start, end)

	def substring(start :Int, end :Int) :String = subSequence(start, end).toString
	final def substring(start :Int) :String = substring(start, length)
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
sealed abstract class ChoppedString extends AbstractSeq[Char] with StringLike with StringLikeOps[ChoppedString] {
//	protected def depth :Int //we can create an efficient stack-based iterator and foreach/map/flatMap
//	override def empty :ChoppedString = ChoppedString.empty

	override def specificFactory :SpecificIterableFactory[Char, ChoppedString] = ChoppedString

//	override def +(string :StringLike) :ChoppedString = string match {
//		case chunk :ChoppedString => this ++ chunk
//		case _ if string.length == 0 => this
//		case _ if length == 0 => specificFactory.fromSpecific(string)
//		case _ => (newSpecificBuilder ++= this ++= string).result()
//	}
//
	@inline final def ++(string :String) :ChoppedString = concat(string)
	@inline final def ++(string :StringLike) :ChoppedString = concat(string)
	override def concat(string :String) :ChoppedString = this appendedAll string
	override def concat(string :StringLike) :ChoppedString = this appendedAll string

	override def appended(char :Char) :ChoppedString = appendedAll(stringOf(char))

	override def appendedAll(string :String) :ChoppedString =
		if (string == null) new AppendedString(this, "null")
		else if (string.length == 0) this
		else if (isEmpty) Substring(string)
		else new AppendedString(this, string)

	override def appendedAll(string :StringLike) :ChoppedString = string match {
//		case null                   => appendedAll("null")
		case _ if length == 0       => ChoppedString.from(string)
		case _ if string.isEmpty    => this
//		case chopped :ChoppedString => appendedAll(chopped)
		case chopped :ChoppedString => new ConcatChunks(this, chopped)
		case _                      => appendedAll(string.toString)
	}
	override def appendedAll(other :IterableOnce[Char]) :ChoppedString = appendedAll(ChoppedString.from(other))
//
//	@inline final def :++(string :ChoppedString) :ChoppedString = appendedAll(string)
//	def appendedAll(string :ChoppedString) :ChoppedString =
//		if (string.isEmpty) this
//		else if (isEmpty) string
//		else new ConcatChunks(this, string)

	override def prepended(char :Char) :ChoppedString = prependedAll(stringOf(char))

	override def prependedAll(string :String) :ChoppedString =
		if (string == null) new PrependedString("null", this)
		else if (string.length == 0) this
		else if (isEmpty) Substring(string)
		else new PrependedString(string, this)

	override def prependedAll(string :StringLike) :ChoppedString = string match {
//		case null                   => prependedAll("null")
		case _ if length == 0       => ChoppedString.from(string)
		case _ if string.isEmpty    => this
//		case chopped :ChoppedString => prependedAll(chopped)
		case chopped :ChoppedString => new ConcatChunks(chopped, this)
		case _                      => prependedAll(string.toString)
	}
	override def prependedAll(other :IterableOnce[Char]) :ChoppedString = prependedAll(ChoppedString.from(other))
//
//	@inline final def ++:(string :ChoppedString) :ChoppedString = prependedAll(string)
//	def prependedAll(string :ChoppedString) :ChoppedString =
//		if (string.isEmpty) this
//		else if (isEmpty) string
//		else new ConcatChunks(string, this)

	@inline final def *(n :Int) :ChoppedString = times(n)
	def times(n :Int) :ChoppedString =
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

	override def substring(start :Int, end :Int) :String =
		if (start < 0 | end < 0 | end < start | end > length)
			throw new IndexOutOfBoundsException("ChoppedString|" + length + "|.substring(" + start + ", " + end + ")")
		else {
			val res = new JStringBuilder(length)
			@tailrec
			def slice(in :Any, from :Int, until :Int, suffix :mutable.Queue[Any], last :Any, lastUntil :Int) :Unit =
				in match {
					case chops  :ChoppedString if from <= 0 && until >= chops.length =>
						res append chops.toString
						if (suffix != null && suffix.nonEmpty)
							slice(suffix.dequeue(), 0, Int.MaxValue, suffix, last, lastUntil)
						else if (last != null)
							slice(last, 0, lastUntil, null, null, 0)
					case concat :Chops =>
						val prefixLen = concat.prefixLength
						if (until <= prefixLen)
							slice(concat.prefix, from, until, suffix, last, lastUntil)
						else if (from >= prefixLen)
							slice(concat.suffix, from - prefixLen, until - prefixLen, suffix, last, lastUntil)
						else if (last == null)
							slice(concat.prefix, from, prefixLen, null, concat.suffix, until - prefixLen)
						else {
							val queue = if (suffix == null) mutable.Queue.empty[Any] else suffix
							slice(concat.prefix, from, prefixLen, concat.suffix +=: queue, last, lastUntil)
						}
					case _ =>
						in match {
							case s :ChoppedString => s.appendTo(res, from, until)
							case s :String        => res append s.substring(from, until)
							case _                => neitherStringNorChoppedString(in)
						}
						if (suffix != null && suffix.nonEmpty)
							slice(suffix.dequeue(), 0, Int.MaxValue, suffix, last, lastUntil)
						else if (last != null)
							slice(last, 0, lastUntil, null, null, 0)
				}
			slice(this, start, end, null, null, 0)
			res.toString
		}

	private def neitherStringNorChoppedString(x :Any) :Nothing =
		throw new AssertionError(
			"Neither a String nor a ChoppedString: " + x + ": " + x.className + "."
		)

	protected override def clippedSlice(from :Int, until :Int) :ChoppedString = {
		@tailrec def slice(in :Any, from :Int, until :Int,
		                   prefix :ChoppedString, suffix :mutable.Queue[Any], last :Any, lastUntil :Int) :ChoppedString =
			in match {
				case chops  :ChoppedString if from <= 0 && until >= chops.length =>
					if (suffix != null && suffix.nonEmpty)
						slice(suffix.dequeue(), 0, Int.MaxValue, prefix appendedAll chops, suffix, last, lastUntil)
					else if (last != null)
						slice(last, 0, lastUntil, prefix appendedAll chops, null, null, 0)
					else
						prefix appendedAll chops
				case concat :Chops =>
					val prefixLen = concat.prefixLength
					if (until <= prefixLen)
						slice(concat.prefix, from, until, prefix, suffix, last, lastUntil)
					else if (from >= prefixLen)
						slice(concat.suffix, from - prefixLen, until - prefixLen, prefix, suffix, last, lastUntil)
					else if (last == null)
						slice(concat.prefix, from, prefixLen, prefix, null, concat.suffix, until - prefixLen)
					else {
						val queue = if (suffix == null) mutable.Queue.empty[Any] else suffix
						slice(concat.prefix, from, prefixLen, prefix, concat.suffix +=: queue, last, lastUntil)
					}
				case _ =>
					val newPrefix = in match {
						case s :ChoppedString => prefix appendedAll s.clippedSlice(from, until)
						case s :String        => prefix appendedAll s.slice(from, until)
						case _                => neitherStringNorChoppedString(in)
					}
					if (suffix != null && suffix.nonEmpty)
						slice(suffix.dequeue(), 0, Int.MaxValue, newPrefix, suffix, last, lastUntil)
					else if (last != null)
						slice(last, 0, lastUntil, newPrefix, null, null, 0)
					else
						newPrefix
			}
		slice(this, from, until, Empty, null, null, 0)
	}

	override def segmentLength(p :Char => Boolean, from :Int) :Int = {
		@tailrec def segment(in :Any, idx :Int, prefix :Int, suffix :mutable.Queue[Any]) :Int = in match {
			case concat :Chops =>
				val prefixSize = concat.prefixLength
				if (idx >= prefixSize)
					segment(concat.suffix, idx - prefixSize, prefix, suffix)
				else {
					val queue = if (suffix == null) mutable.Queue.empty[Any] else suffix
					segment(concat.prefix, idx, prefix, concat.suffix +=: queue)
				}
			case chops :ChoppedString =>
				val len = chops.segmentLength(p, idx)
				if (suffix == null || suffix.isEmpty || len < chops.length - idx) prefix + len
				else segment(suffix.dequeue(), 0, prefix + len, suffix)
			case s :String =>
				val len = s.segmentLength(p, idx)
				if (suffix == null || suffix.isEmpty || len < s.length - idx) prefix + len
				else segment(suffix.dequeue(), 0, prefix + len, suffix)
			case _ => neitherStringNorChoppedString(in)
		}
		if (from >= length) 0
		else if (from < 0) segment(this, 0, 0, null)
		else segment(this, from, 0, null)
	}

	override def apply(i :Int) :Char = {
		@tailrec def rec(chops :ChoppedString, idx :Int) :Char = chops match {
			case concat    :ConcatChunks    =>
				val prefixSize = concat.prefix.length
				if (idx < prefixSize) rec(concat.prefix, idx)
				else rec(concat.suffix, idx - prefixSize)
			case appended  :AppendedString  =>
				val prefix = appended.prefix
				if (idx < prefix.length) rec(prefix, idx)
				else appended.suffix.charAt(idx - prefix.length)
			case prepended :PrependedString =>
				val prefix = prepended.prefix
				if (idx < prefix.length) prefix(idx)
				else rec(prepended.suffix, idx - prefix.length)
			case _ =>
				chops(idx)
		}
		rec(this, i)
	}

	override def updated(index :Int, elem :Char) :ChoppedString =
		if (index < 0 || index >= length)
			throw new IndexOutOfBoundsException(index.toString + " out of " + length)
		else if (index == 0)
			new PrependedString(elem.toString, drop(1))
		else if (index == length - 1)
			new AppendedString(dropRight(1), elem.toString)
		else
			new ConcatChunks(new AppendedString(take(index), elem.toString), drop(index + 1))

	def patch(from :Int, other :IterableOnce[Char], replaced :Int) :ChoppedString =
		if (from >= length) appendedAll(other)
		else if (from <= 0) drop(replaced) prependedAll other
		else if (replaced <= 0) take(from) appendedAll other appendedAll drop(from)
		else if (replaced >= length - from) take(from) appendedAll other
		else take(from) appendedAll other appendedAll drop(from + replaced)

	def patch(from :Int, other :String, replaced :Int) :ChoppedString =
		if (from >= length) appendedAll(other)
		else if (from <= 0) drop(replaced) prependedAll other
		else if (replaced <= 0) take(from) appendedAll other appendedAll drop(from)
		else if (replaced >= length - from) take(from) appendedAll other
		else take(from) appendedAll other appendedAll drop(from + replaced)


	override def foreach[U](f :Char => U) :Unit = foldLeft(())((_, char) => f(char))

	override def foldLeft[A](z :A)(op :(A, Char) => A) :A = {
		@tailrec def fold(acc :A, in :Any, suffix :mutable.Queue[Any]) :A = in match {
			case concat    :ConcatChunks =>
				val suffix0 = if (suffix == null) mutable.Queue.empty[Any] else suffix
				fold(acc, concat.prefix, concat.suffix +=: suffix0)
			case appended  :AppendedString =>
				val suffix0 = if (suffix == null) mutable.Queue.empty[Any] else suffix
				fold(acc, appended.prefix, appended.suffix +=: suffix0)
			case prepended :PrependedString =>
				val a = prepended.prefix.foldLeft(acc)(op)
				fold(a, prepended.suffix, suffix)
			case chops     :ChoppedString =>
				val a = chops.foldLeft(acc)(op)
				if (suffix == null || suffix.isEmpty) a
				else fold(a, suffix.dequeue(), suffix)
			case string    :String =>
				val a = string.foldLeft(acc)(op)
				if (suffix == null || suffix.isEmpty) a
				else fold(a, suffix.dequeue(), suffix)
			case _ =>
				neitherStringNorChoppedString(in)
		}
		fold(z, this, null)
	}

	override def foldRight[A](z :A)(op :(Char, A) => A) :A = {
		@tailrec def fold(acc :A, in :Any, prefix :Buffer[Any]) :A = in match {
			case concat    :ConcatChunks =>
				val prefix0 = if (prefix == null) Buffer.empty[Any] else prefix
				fold(acc, concat.suffix, prefix0 += concat.prefix)
			case prepended :PrependedString =>
				val prefix0 = if (prefix == null) Buffer.empty[Any] else prefix
				fold(acc, prepended.suffix, prefix0 += prepended.prefix)
			case appended  :AppendedString =>
				val a = appended.suffix.foldRight(acc)(op)
				fold(a, appended.prefix, prefix)
			case chops     :ChoppedString =>
				val a = chops.foldRight(acc)(op)
				if (prefix == null || prefix.isEmpty) a
				else fold(a, prefix.remove(prefix.length - 1), prefix)
			case string    :String =>
				val a = string.foldRight(acc)(op)
				if (prefix == null || prefix.isEmpty) a
				else fold(a, prefix.remove(prefix.length - 1), prefix)
			case _ =>
				neitherStringNorChoppedString(in)
		}
		fold(z, this, null)
	}

	@inline private def cat(i1 :Iterator[Char], i2 :Iterator[Char]) :Iterator[Char] =
		if (i1.hasNext)
			if (i2.hasNext) i1 ++ i2 else i1
		else
			i2

	override def iterator :Iterator[Char] = {
		@tailrec def rec(s :ChoppedString, prefix :Iterator[Char], suffix :Iterator[Char]) :Iterator[Char] =
			s match {
				case append   :AppendedString  =>
					rec(append.prefix, prefix, cat(append.suffix.iterator, suffix))
				case prepend  :PrependedString =>
					rec(prepend.suffix, cat(prefix, prepend.prefix.iterator), suffix)
				case concat   :ConcatChunks    =>
					rec(concat.suffix, prefix ++ concat.prefix.iterator, suffix)
				case straight :Substring if straight.length > 0 =>
					cat(cat(prefix, straight.iterator), suffix)
				case _ =>
					cat(prefix, suffix)
			}
		rec(this, Iterator.empty, Iterator.empty)
	}

	override def reverseIterator :Iterator[Char] = {
		@tailrec def rec(s :ChoppedString, prefix :Iterator[Char], suffix :Iterator[Char]) :Iterator[Char] =
			s match {
				case append   :AppendedString  =>
					rec(append.prefix, cat(prefix, append.suffix.reverseIterator), suffix)
				case prepend  :PrependedString =>
					rec(prepend.suffix, prefix, cat(prepend.prefix.reverseIterator, suffix))
				case concat   :ConcatChunks    =>
					rec(concat.prefix, prefix ++ concat.suffix.reverseIterator, suffix)
				case straight :Substring if straight.length > 0 =>
					cat(cat(prefix, straight.reverseIterator), suffix)
				case _ =>
					cat(prefix, suffix)
			}
		rec(this, Iterator.empty, Iterator.empty)
	}

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S = shape.shape match {
		case CharShape | IntShape => intStepper(this, Stepper.ofInt(), Stepper.ofInt()).asInstanceOf[S]
		case _ => super.stepper
	}
	@tailrec private def intStepper(s :ChoppedString, prefix :IntStepper, suffix :IntStepper) :IntStepper =
		s match {
			case append   :AppendedString  =>
				intStepper(append.prefix, prefix, new StringStepper(append.suffix) :++ suffix)
			case prepend  :PrependedString =>
				intStepper(prepend.suffix, prefix :++ new StringStepper(prepend.prefix), suffix)
			case concat   :ConcatChunks    =>
				intStepper(concat.suffix, prefix ++ concat.prefix.stepper, suffix)
			case straight :Substring if straight.length > 0 =>
				prefix :++ straight.stepper :++ suffix
			case _ =>
				prefix :++ suffix
		}
//
//	private def lengthOf(string :Any) :Int = string match {
//		case string :ChoppedString => string.length
//		case string :String        => string.length
//		case _                     =>
//			throw new AssertionError(
//				"Neither a String nor a ChoppedString: " + string + ": " + string.className + "."
//			)
//	}

	def isWhitespace :Boolean = {
		val i = javaIterator
		var isWhite = true
		while (i.hasNext && { isWhite = Character.isWhitespace(i.next().toChar); isWhite })
			{}
		isWhite
	}
	def isAlphanumeric :Boolean = {
		val i = javaIterator
		var isAlphanum = true
		while (i.hasNext && { isAlphanum = Character.isLetterOrDigit(i.next()); isAlphanum })
			{}
		isAlphanum
	}

	override def className :String = "ChoppedString"

	//todo: test if it's faster to append a String char by char, or append a whole substring
	protected def appendTo(builder :JStringBuilder) :JStringBuilder
	protected def appendTo(builder :JStringBuilder, from :Int, until :Int) :JStringBuilder

	override def toReader :Reader = new ChoppedStringReader(this)

	override def toString :String = appendTo(new JStringBuilder).toString
}




/**
  * $factoryInfo
  * @define Coll `ChoppedString`
  * @define coll chopped string
  */
object ChoppedString extends SpecificIterableFactory[Char, ChoppedString] {
	override val empty :ChoppedString = Empty

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
		case ArrayLike.Wrapped.Slice(arr :Array[Char], from, until) =>
			var i = from
			val res = new lang.StringBuilder(until - from)
			while (i < until) {
				res append arr(i)
				i += 1
			}
			ChoppedString(res.toString)
		case it :Iterable[Char] => //May avoid boxing if it has a fixed element type Char
			val size = it.knownSize
			if (size == 0)
				Empty
			else {
				val res =
					if (size > 0) new java.lang.StringBuilder(size)
					else new lang.StringBuilder
				it foreach res.append
				Substring(res.toString)
			}
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

	def chunkedBuilder :Builder[String, ChoppedString] = new ReusableBuilder[String, ChoppedString] {
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

	private def stringOf(char :Char) :String = {
		val byte = char & 0xff
		if (byte == char) SingletonStrings(byte) else String.valueOf(char)
	}
	private[this] final val SingletonStrings = Array.tabulate(256) { i => String.valueOf(i.toChar) }


	@SerialVersionUID(Ver)
	private object Empty extends ChoppedString with EmptySeqOps[Char, Seq, ChoppedString] {
		override def isEmpty = true
		override def intIterator = JavaIterator.ofInt()

		protected override def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder = builder
		protected override def appendTo(builder :JStringBuilder, from :Int, until :Int) :JStringBuilder = builder
		override def toReader = Reader.nullReader()
		override def toString = ""
	}


	private abstract class Chops extends ChoppedString {
		def prefixLength :Int
		def prefix :Any
		def suffix :Any
	}

	@SerialVersionUID(Ver)
	private class AppendedString(override val prefix :ChoppedString, override val suffix :String) extends Chops {
		assert(suffix.length > 0, "Appended an empty String to " + suffix + ".")
		override val length       = prefix.length + suffix.length
		override def prefixLength = prefix.length
		override def head = if (prefix.isEmpty) suffix.charAt(0) else prefix.head
		override def last = suffix.charAt(suffix.length - 1)

		override def apply(i :Int) :Char = if (i < prefix.length) prefix(i) else suffix.charAt(i - prefix.length)

		protected override def appendTo(builder :java.lang.StringBuilder) =
			prefix appendTo builder append suffix

		protected override def appendTo(builder :java.lang.StringBuilder, from :Int, until :Int) = {
			val prefixLength = prefix.length
			if (until <= prefixLength)
				prefix.appendTo(builder, from, until)
			else if (from >= prefixLength)
				builder append suffix.substring(from - prefixLength, until - prefixLength)
			else
				suffix.appendTo(prefix.appendTo(builder, from, prefixLength), 0, until - prefixLength)
		}

		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}


	@SerialVersionUID(Ver)
	private class PrependedString(override val prefix :String, override val suffix :ChoppedString) extends Chops {
		assert(prefix.length > 0, "Prepended an empty String to " + suffix + ".")
		override val length       = prefix.length + suffix.length
		override def prefixLength = prefix.length
		override def head = prefix.charAt(0)
		override def last = if (suffix.isEmpty) prefix.charAt(prefix.length - 1) else suffix.last
		override def apply(idx :Int) :Char = if (idx < prefix.length) prefix.charAt(idx) else suffix(idx - prefix.length)

		override protected def appendTo(builder :java.lang.StringBuilder) =
			suffix appendTo (builder append prefix)

		protected override def appendTo(builder :java.lang.StringBuilder, from :Int, until :Int) = {
			val prefixLength = prefix.length
			if (until <= prefixLength)
				builder append prefix.substring(from, until)
			else if (from >= prefixLength)
				suffix.appendTo(builder, from - prefixLength, until - prefixLength)
			else
				suffix.appendTo(builder append prefix.substring(from, prefixLength), 0, until - prefixLength)
		}

		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}


	@SerialVersionUID(Ver)
	private class ConcatChunks(override val prefix :ChoppedString, override val suffix :ChoppedString) extends Chops {
		assert(prefix.nonEmpty && suffix.nonEmpty,
			"Concatenation with an empty String: " + prefix.length + " + " + suffix.length + "."
		)
		override val length       = prefix.length + suffix.length
		override def prefixLength = prefix.length
		override def head = if (prefix.isEmpty) suffix.head else prefix.head
		override def last = if (suffix.isEmpty) prefix.last else suffix.last
		override def apply(idx :Int) = if (idx < prefix.length) prefix(idx) else suffix(idx - prefix.length)

		override def appendTo(builder :java.lang.StringBuilder) =
			suffix.appendTo(prefix.appendTo(builder))

		protected override def appendTo(builder :java.lang.StringBuilder, from :Int, until :Int) = {
			val prefixLength = prefix.length
			if (until <= prefixLength)
				prefix.appendTo(builder, from, until)
			else if (from >= prefixLength)
				suffix.appendTo(builder, from - prefixLength, until - prefixLength)
			else
				suffix.appendTo(prefix.appendTo(builder, from, prefixLength), 0, until - prefixLength)
		}

		override lazy val toString :String = super.toString
		private def writeReplace :ChoppedString = Substring(toString)
	}



	private final class ChoppedStringBuilder extends ReusableBuilder[Char, ChoppedString] {
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
//			case _  :View[Char] => addAll(xs.iterator)
			case it :Iterable[Char] if it.knownSize == 0 =>
				this
			case chunk :ChoppedString =>
				flush()
				if (chunks == null) chunks = chunk
				else chunks ++= chunk
				this
			case str :WrappedString => addAll(str.unwrap); this
			case ErasedArray.Wrapped(a :Array[Char]) => addSlice(a, 0, a.length); this
			case ErasedArray.Wrapped.Slice(a :Array[Char], from, until) => addSlice(a, from, until); this
			case it :Iterable[Char] => addAll(it); this
			case it                 => addAll(it.iterator); this //higher chance of JIT kicking in when extracted
		}

		private[this] def addAll(string :String) :Unit =
			if (chunkBuilder != null && string.length <= chunkBuilder.capacity - chunkBuilder.length)
				chunkBuilder append string
			else {
				flush()
				if (chunks == null)
					chunks = Substring(string)
				else
					chunks ++= string
			}

		private[this] def addSlice(chars :Array[Char], from :Int, until :Int) :Unit =
			if (chunkBuilder != null && until - from <= chunkBuilder.capacity - chunkBuilder.length) {
				var i = from
				while (i < until) {
					chunkBuilder append chars(i)
					i += 1
				}
			} else {
				flush()
				val string = String.valueOf(chars, from, until - from)
				if (chunks == null) chunks   = Substring(string)
				else                chunks ++= string
			}

		private[this] def addAll(it :Iterable[Char]) :Unit = {
			val size_? = it.knownSize
			if (chunkBuilder == null)
				chunkBuilder =
					if (size_? > 0) new java.lang.StringBuilder(size_?)
					else new java.lang.StringBuilder
			else if (size_? > 0)
				chunkBuilder ensureCapacity chunkBuilder.length + size_?
//			it foreach chunkBuilder.append //boxing in every call
			val stepper = it.stepper
			while (stepper.hasStep)
				chunkBuilder append stepper.nextStep().toChar
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

		private def flush() :Unit = {
			hinted = false
			if (chunkBuilder != null && chunkBuilder.length > 0) {
				if (chunks == null) chunks = Substring(chunkBuilder.toString)
				else chunks ++= chunkBuilder.toString
				chunkBuilder = null
			}
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
		override def clear() :Unit = { chunkBuilder = null; chunks = null; hinted = false }
	}


	private class ChoppedStringReader(private[this] var tail :ChoppedString) extends Reader {
		override def read :Int = {
			if (tail.isEmpty)
				throw new IOException("empty string")
			val h = tail.head
			tail = tail.tail
			h
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






private[collections] trait SubstringOps[C <: StringLike with IndexedSeq[Char]]
	extends IndexedSeq[Char] with IndexedSeqOps[Char, IndexedSeq, C]
	   with SugaredSlicingOps[Char, IndexedSeq, C] with SpecificIterableFactoryDefaults[Char, IndexedSeq, C]
{ this :C =>
	protected def whole :String
	protected def startIndex :Int
	protected def fromString(string :String, from :Int, until :Int) :C

	protected override def clippedSlice(from :Int, until :Int) :C = {
		val start = startIndex
		fromString(whole, start + from, start + until)
	}

	override def head :Char =
		if (length == 0) throw new NoSuchElementException("Substring().head")
		else whole.charAt(startIndex)

	override def last :Char = {
		val length = this.length
		if (length == 0) throw new NoSuchElementException("Substring().last")
		else whole.charAt(startIndex + length - 1)
	}
	override def apply(i :Int) :Char =
		if (i < 0 || i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			whole.charAt(startIndex + i)

	override def reverse :C = {
		val string = whole
		val offset = startIndex
		val sb     = new java.lang.StringBuilder(length)
		var i      = length
		while (i > offset) {
			i -= 1
			sb append string.charAt(i)
		}
		fromString(sb.toString, 0, length)
	}

	override def iterator :StringIterator = {
		val start = startIndex
		new StringIterator(whole, start, start + length)
	}
	override def reverseIterator :ReverseStringIterator = {
		val start = startIndex
		new ReverseStringIterator(whole, start, start + length)
	}
	override def intIterator :JIntIterator = {
		val start = startIndex
		JavaIterator.slice(whole, start, start + length)
	}
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Char, S]) :S with EfficientSplit = {
		val length = this.length
		val start  = this.startIndex
		if (length == 0) Stepper0()
		else shape.shape match {
			case CharShape | IntShape =>
				new StringStepper(whole, start, start + length).castFrom[IntStepper with EfficientSplit, S with EfficientSplit]
			case _ =>
				super.stepper
		}
	}
	override def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
		intIterator.asInstanceOf[I]

	override def foldLeft[A](z :A)(op :(A, Char) => A) :A = {
		val str = whole
		var res = z
		var i   = startIndex
		val end = i + length
		while (i < end) {
			res = op(res, str.charAt(i))
			i += 1
		}
		res
	}
	override def foldRight[A](z :A)(op :(Char, A) => A) :A = {
		val str = whole
		var res = z
		val end = startIndex
		var i   = end + length
		while (i > end) {
			i -= 1
			res = op(str.charAt(i), res)
		}
		res
	}
	override def foreach[U](f :Char => U) :Unit = {
		val whole = this.whole
		var i     = startIndex
		val end   = i + length
		while (i < end) {
			f(whole.charAt(i))
			i += 1
		}
	}

	override def segmentLength(p :Char => Boolean, from :Int) :Int = {
		val length = this.length
		if (from >= length)
			0
		else {
			val whole = this.whole
			val end   = startIndex + length
			val start = startIndex + math.max(from, 0)
			var i     = start
			while (i < end && p(whole.charAt(i)))
				i += 1
			i - start
		}
	}

	override def toReader :Reader =
		if (length == whole.length) new StringReader(whole)
		else new SubstringReader(whole, startIndex, length)

	override def equals(that :Any) :Boolean = that match {
		case other :SubstringOps[_] if other.canEqual(this) && canEqual(other) =>
			length == other.length && {
				val string      = whole
				val otherString = other.whole
				var i = startIndex
				var j = other.startIndex
				val end = i + length
				while (i < end && string.charAt(i) == otherString.charAt(j)) {
					i += 1; j += 1
				}
				i == end
			}
		case _ => super.equals(that)
	}

	override def toString :String = {
		val start = startIndex
		whole.substring(start, start + length)
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
final class Substring private (protected override val whole :String,
                               protected override val startIndex :Int, override val length :Int)
	extends ChoppedString with SubstringOps[Substring]
{
	assert(startIndex >= 0 & startIndex <= whole.length,
		"Offset out of bounds: \"" + whole + "\".substring(" + startIndex + "->" + length + ")."
	)
	assert(length <= whole.length - startIndex,
		"Length out of bounds: \"" + whole + "\".substring(" + startIndex + "->" + length + ")."
	)
	override def specificFactory :SpecificIterableFactory[Char, Substring] = Substring

	protected override def fromString(string :String, from :Int, until :Int) :Substring =
		new Substring(string, from, until - from)

//	override def substring(start :Int, end :Int = length) :Substring = slice(start, end)
	override def substring(start :Int, end :Int) :String =
		if (start < 0 | end < 0 | end < start | end > length)
			throw new IndexOutOfBoundsException("Substring|" + length + "|.substring(" + start + ", " + end + ")")
		else
			whole.substring(startIndex + start, startIndex + end)

	override def isWhitespace :Boolean = {
		val string = whole
		var i      = startIndex
		val until  = i + length
		while (i < until && Character.isWhitespace(string.charAt(i)))
			i += 1
		i == until
	}

	override def appendedAll(string :StringLike) :ChoppedString = string match {
//		case null                => appendedAll("null")
		case _ if length == 0    => ChoppedString.from(string)
		case _ if string.isEmpty => this
		case Substring(data, from, until) if (this.whole eq data) && from == startIndex + length =>
			new Substring(this.whole, startIndex, length + until - from)
		case _                   => super.appendedAll(string)
	}
	override def prependedAll(string :StringLike) :ChoppedString = string match {
//		case null                => prependedAll("null")
		case _ if length == 0    => ChoppedString.from(string)
		case _ if string.isEmpty => this
		case Substring(data, from, until) if (this.whole eq data) && until == startIndex =>
			new Substring(this.whole, from, until - from + length)
		case _                   => super.prependedAll(string)
	}

	protected override def appendTo(builder :JStringBuilder) :JStringBuilder = appendTo(builder, 0, length)
	protected override def appendTo(builder :JStringBuilder, from :Int, until :Int) :JStringBuilder = {
		val string = whole
		var i      = startIndex + from
		val end    = startIndex + until
		while (i < end) {
			builder append string.charAt(i)
			i += 1
		}
		builder
	}

	private def writeReplace = new Substring(whole.substring(startIndex, startIndex + length), 0, length)
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
		case string :Substring => Got((string.whole, string.startIndex, string.startIndex + string.length))
		case _ => Lack
	}
	/** If `chars` is a [[net.noresttherein.sugar.collections.Substring! Substring]], extracts its underlying string
	  * as well as the indices of the first character and the character following the last character.
	  */
	def unapply(chars :CharSequence) :Opt[(String, Int, Int)] = chars match {
		case string :Substring => Got((string.whole, string.startIndex, string.startIndex + string.length))
		case _ => Lack
	}
	/** If `chars` is a [[net.noresttherein.sugar.collections.Substring! Substring]], extracts its underlying string
	  * as well as the indices of the first character and the character following the last character.
	  */
	@inline def unapply(chars :StringLike) :Opt[(String, Int, Int)] = unapply(chars :CharSequence)


	override def newBuilder :Builder[Char, Substring] = new StringBuilder().mapResult(apply _)

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
