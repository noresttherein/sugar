package net.noresttherein.sugar.collections

import java.io.{Reader, StringReader}

import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, WrappedString}
import scala.collection.{SpecificIterableFactory, Stepper, StepperShape}
import scala.collection.mutable.Builder
import scala.collection.Stepper.EfficientSplit

import net.noresttherein.sugar.JavaTypes.{JIntIterator, JIterator}
//import net.noresttherein.sugar.collections.ArrayText.StringText




/**
  * @author Marcin Mościcki
  */
/*
trait ArrayText
	extends IndexedSeq[Char] with StringLike
	   with IndexedSeqOps[Char, IndexedSeq, ArrayText] with StringLikeOps[ArrayText]
	   with SpecificIterableOps[Char, IndexedSeq, ArrayText]
{
	override def +(char :Char) :ArrayText

	override def +(string :String) :StringLike =
		if (string.length == 0)
			this
		else if (length == 0)
			new StringText(string, 0, string.length)
		else {
			val b = ArrayText.newBuilder
			b sizeHint length + string.length
			(b ++= this ++= string).result()
		}

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Char, I]) :I =
		intIterator.asInstanceOf[I]

	protected override def specificFactory :SpecificIterableFactory[Char, ArrayText] = ArrayText

	override def className = "ArrayText"
}


@SerialVersionUID(ver)
object ArrayText extends SpecificIterableFactory[Char, ArrayText] {
	override def empty :ArrayText = ArrayText0

	def single(char :Char) :ArrayText = new ArrayText1(char)

	def one(char :Char) :ArrayText = new ArrayText1(char)

	def two(first :Char, second :Char) :ArrayText = new ArrayText2(first, second)

	override def fromSpecific(it :IterableOnce[Char]) :ArrayText = it match {
		case text   :ArrayText => text
		case empty  :Iterable[_] if empty.isEmpty => ArrayText0
		case string :WrappedString => new StringText(string.toString,)
//		case arr    :PassedArray[Char] =>
		case empty :Iterator[_] if !empty.hasNext => ArrayText0
		case _ => (newBuilder ++= it).result()
	}
	override def newBuilder :Builder[Char, ArrayText] = ???


	private object ArrayText0 extends PassedArray0 with ArrayText {
		override def +(char :Char) = single(char)
		override def +(string :String) = new StringText(string)

		override def intIterator = JavaIterator.ofInt()
		override def toReader = Reader.nullReader()

		override def trustedSlice(from :Int, until :Int) = this
		override def subSequence(start :Int, end :Int) = this
	}

	private trait PassedArrayText
		extends PassedArray[Char] with IndexedSeqOps[Char, PassedArray, PassedArrayText]
		   with ArrayText with SpecificIterableOps[Char, PassedArray, PassedArrayText]
	{
		override private[collections] def elementType :Class[_] = classOf[Char]

//		override def +(char :Char) :ArrayText =


		override def appended[B >: Char](elem :B) :PassedArray[B] =
			if (elem.isInstanceOf[Char]) (this + elem.asInstanceOf[Char]).asInstanceOf[PassedArray[B]]
			else super.appended(elem)


	}


	@SerialVersionUID(ver)
	private class ArrayText1(c :Char) extends PassedArray1[Char](c) with PassedArrayText {
		override def trustedSlice(from :Int, until :Int) :ArrayText =

		override def intIterator = JavaIterator.ofInt(c)

		override def toReader = new Reader {
			private var hasNext = true
			override def read(cbuf :Array[Char], off :Int, len :Int) =
				if (hasNext)
					if (off >= 0 & off < cbuf.length & off < len & len < cbuf.length) {
						cbuf(off) = c
						hasNext = false
						1
					} else
						throw new IndexOutOfBoundsException(
							"[" + off + ".." + (off + length) + ") from [0.." + cbuf.length + ")"
						)
				else -1

			override def close() = ()
		}
	}

	@SerialVersionUID(ver)
	private class ArrayText2(first :Char, second :Char)
		extends PassedArray2[Char](first, second, classOf[Char]) with PassedArrayText
	{
		override def intIterator = JavaIterator.ofInt(first, second)

		override def toReader = new StringReader(first.toString + second)
	}

	@SerialVersionUID(ver)
	private class ArrayTextPlus(chars :Array[Char], offset :Int, length :Int, owner :Boolean = false)
		extends PassedArrayPlus(chars, offset, length, owner) with PassedArrayText
	{

	}


	@SerialVersionUID(ver)
	private class StringText(text :String, offset :Int, override val length :Int)
		extends AbstractSubstring[ArrayText](text, offset, length) with ArrayText
	{
		def this(text :String) = this(text, 0, text.length)

		override def +(char :Char) :ArrayText = (ArrayText.newBuilder ++= this += char).result()

		protected override def trustedSlice(from :Int, until :Int) :ArrayText = {
			val slice = new StringText(text, offset + from, until - from)
			if (slice.length >= length / 4) slice
			else (ArrayText.newBuilder ++= slice).result()
		}
	}

	private final val SliceReallocationFactor = 4

}
*/
