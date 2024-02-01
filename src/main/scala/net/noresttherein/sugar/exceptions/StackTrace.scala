package net.noresttherein.sugar.exceptions

import java.io.{PrintStream, PrintWriter}

import scala.collection.SpecificIterableFactory
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.arrays.IArray
import net.noresttherein.sugar.collections.{ArraySliceOps, SpecificIterableFactoryDefaults, SugaredIterable}
import net.noresttherein.sugar.concurrent.releaseFence




/** A specific sequence class for stack frames, used as a replacement for [[Throwable.getStackTrace getStackTrace]]
  * in [[net.noresttherein.sugar.exceptions.SugaredThrowable SugaredThrowable]] and
  * [[net.noresttherein.sugar.exceptions.extensions.ThrowableExtension extension methods]] for regular exceptions.
  */
//  * @define Coll `StackTrace`
//  * @define coll stack trace
@SerialVersionUID(Ver)
abstract class StackTrace
	extends AbstractSeq[StackTraceElement]
	   with IndexedSeq[StackTraceElement] with IndexedSeqOps[StackTraceElement, IndexedSeq, StackTrace]
	   with SugaredIterable[StackTraceElement]
	   with SpecificIterableFactoryDefaults[StackTraceElement, IndexedSeq, StackTrace] with DefaultSerializable
{
	/** The most recent activation frame.
	  * @return `head` */
	def top    :StackTraceElement = head

	/** The first activation frame of the associated `Thread`.
	  * @return `last` */
	def bottom :StackTraceElement = last

	/** The longest shared suffix of the two stack traces, starting with the call to the method
	  * within which the calls diverged.
	  */
	def sharedFrames(other :StackTrace) :StackTrace =
		slice(length - sharedSuffixLength(other), length)

	/** Drops the longest shared suffix with the other stack trace. */
	def dropSharedFrames(other :StackTrace) :StackTrace =
		slice(0, length - sharedSuffixLength(other))

	/** The length of the longest shared suffix of the two stack traces, starting with the call to the method
	  * within which the calls diverged.
	  */
	def sharedSuffixLength(other :StackTrace) :Int =
		if (isEmpty || other.isEmpty)
			0
		else if (last != other.last)
			0
		else {
			val length       = this.length
			val otherLength  = other.length
			val sharedLength = math.min(length, otherLength)
			var unmatched    = sharedLength - 1
			var matched      = 0
			while (matched != unmatched) {
				val middle = (matched + unmatched + 1) >>> 1
				if (this(length - middle) == other(otherLength - middle))
					matched = middle
				else
					unmatched = middle - 1
			}
			matched
		}

	//todo: zip with another stack trace
//	def zip(seq :Seq[StackContext]) :Seq[(StackTraceElement, Maybe[StackContext])] = ???


	override def specificFactory :SpecificIterableFactory[StackTraceElement, StackTrace] = StackTrace
	protected override def className :String = "StackTrace"
}


/** $factoryInfo
  * @define Coll `StackTrace`
  * @define coll stack trace
  */
@SerialVersionUID(Ver)
case object StackTrace extends SpecificIterableFactory[StackTraceElement, StackTrace] {
	def apply(e :Throwable) :StackTrace = {
		val a = e.getStackTrace
		if (a == null || a.length == 0) empty
		else new ArrayStackTrace(a)
	}

	def apply(stackTrace :Array[StackTraceElement]) :StackTrace = new ArrayStackTrace(stackTrace)

	override val empty :StackTrace = new ArrayStackTrace(new Array[StackTraceElement](0))

	override def newBuilder :Builder[StackTraceElement, StackTrace] =
		Array.newBuilder[StackTraceElement].mapResult(new ArrayStackTrace(_))

	override def fromSpecific(it :IterableOnce[StackTraceElement]) :StackTrace = it match {
		case stack :StackTrace => stack
		case empty :Iterable[_] if empty.isEmpty => this.empty
		case empty :Iterator[_] if !empty.hasNext => this.empty
		case IArray.Wrapped.Slice(array, from, until) =>
			new ArrayStackTrace(array.asInstanceOf[Array[StackTraceElement]], from, until - from)
		case _ => (newBuilder ++= it).result()
	}

	@inline def unapplySeq(stack :StackTrace) :StackTrace = stack

}




@SerialVersionUID(Ver)
private class ArrayStackTrace(array :Array[StackTraceElement], start :Int, override val length :Int)
	extends StackTrace with ArraySliceOps[StackTraceElement, IndexedSeq, StackTrace]
{
	def this(array :Array[StackTraceElement]) = this(array, 0, array.length)

	releaseFence()
	override def unsafeArray = array
	override def startIndex  = start

	override def apply(i :Int) :StackTraceElement =
		if (i < 0 || i >= length)
			outOfBounds_!(i, length)
		else
			array(start + i)

	protected override def clippedSlice(from :Int, until :Int) :StackTrace =
		new ArrayStackTrace(array, start + from, until - from)

	override def segmentLength(p :StackTraceElement => Boolean, from :Int) =
		if (start + length == array.length) array.segmentLength(p, from)
		else super[StackTrace].segmentLength(p, from)
}
