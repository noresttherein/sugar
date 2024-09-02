package net.noresttherein.sugar.collections

import scala.collection.{IterableOps, View}
import scala.collection.mutable.ArrayBuffer.DefaultInitialSize
import scala.collection.mutable.{ArrayBuffer, Builder, Growable, ImmutableBuilder, ReusableBuilder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{ArrayFactory, ArrayIterator, ErasedArray, RefArray}
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.collections.extensions.{IterableExtension, IterableOnceExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.exceptions.{illegalState_!, maxSize_!}




private class ComposedBuilder[-X, Y, C, +R](builder :Builder[Y, C], mapArgs :X => Y, mapRes :C => R)
	extends Builder[X, R]
{
	override def knownSize :Int = builder.knownSize
	override def sizeHint(size :Int) :Unit = builder.sizeHint(size)

	override def addOne(elem :X) :this.type = { builder addOne mapArgs(elem); this }
	override def addAll(xs :IterableOnce[X]) :this.type = {
		val size_? = knownSize
		if (size_? >= 0)
			sizeHint(xs, size_?)
		super.addAll(xs)
	}

	override def mapResult[NewTo](f :R => NewTo) :Builder[X, NewTo] =
		new ComposedBuilder(builder, mapArgs, mapRes andThen f)

	def mapInput[Arg](f :Arg => X) :Builder[Arg, R] =
		new ComposedBuilder(builder, f andThen mapArgs, mapRes)

	override def result() :R = mapRes(builder.result())
	override def clear() :Unit = builder.clear()

	protected def underlying :Builder[Y, C] = builder
	protected final def underlyingElement :X => Y = mapArgs
	protected final def mappedResult :C => R = mapRes
}

private class ComposedReusableBuilder[-X, Y, C, +R]
                                     (override val underlying :ReusableBuilder[Y, C], mapArgs :X => Y, mapRes :C => R)
	extends ComposedBuilder[X, Y, C, R](underlying, mapArgs, mapRes) with ReusableBuilder[X, R]
{
	override def mapResult[NewTo](f :R => NewTo) :Builder[X, NewTo] =
		new ComposedReusableBuilder(underlying, underlyingElement, mappedResult andThen f)

	override def mapInput[Arg](f :Arg => X) :Builder[Arg, R] =
		new ComposedReusableBuilder(underlying, f andThen underlyingElement, mappedResult)
}




private class AdditiveBuilder[E, C[X] <: IterableOps[X, C, C[X]]](empty :C[E])
	extends ImmutableBuilder[E, C[E]](empty)
{
	override def addOne(elem :E) :this.type = { elems = elems add elem; this }

	override def addAll(xs :IterableOnce[E]) :this.type = { elems = elems concat xs; this }
}




object Builders {
	trait fromBytes[To] extends Builder[Byte, To] {
		override def addOne(elem :Byte) :this.type
	}
	trait fromChars[To] extends Builder[Char, To] {
		override def addOne(elem :Char) :this.type
	}
	trait fromShorts[To] extends Builder[Short, To] {
		override def addOne(elem :Short) :this.type
	}
	trait fromInts[To] extends Builder[Int, To] {
		override def addOne(elem :Int) :this.type
	}
	trait fromLongs[To] extends Builder[Long, To] {
		override def addOne(elem :Long) :this.type
	}
	trait fromFloats[To] extends Builder[Float, To] {
		override def addOne(elem :Float) :this.type
	}
	trait fromDoubles[To] extends Builder[Double, To] {
		override def addOne(elem :Double) :this.type
	}
	trait fromBooleans[To] extends Builder[Boolean, To] {
		override def addOne(elem :Boolean) :this.type
	}
}




/** A base class for builders of collections backed by arrays.
  * It maintains a growing `Array[ArrayElem]`, which is passed to `result(array :Array[ArrayElem], size :Int)` method
  * when `result()` is called.
  * @define Coll `To`
  * @define coll collection
  */
private[sugar] abstract class ArrayBasedBuilder[ArrayElem :ClassTag, -Elem <: ArrayElem, +To]
	extends ReusableBuilder[Elem, To]
{
	private[this] var array = ArrayFactory.empty[ArrayElem]
	private[this] var size  = 0
	override def knownSize = size

	override def sizeHint(hint :Int) :Unit =
		if (hint > array.length)
			array = Array.copyOf(array, hint)

	protected def ensure(extra :Int) :Unit =
		if (extra > array.length - size) {
			val newSize = math.max(initArraySize, math.max(size + extra, math.min(MaxArraySize >> 1, array.length) << 1))
			if (extra < newSize - size)
				maxSize_!(size, extra, MaxArraySize)
			array = Array.copyOf(array, newSize)
		}

	override def addOne(elem :Elem) :this.type = {
		if (size == array.length)
			ensure(1)
		array(size) = elem
		size += 1
		this
	}
	override def addAll(xs :IterableOnce[Elem]) :this.type = xs.knownSize match {
		case  0 => this
		case -1 => super.addAll(xs)
		case  n =>
			ensure(n)
			val copied = xs.toBasicOps.copyToArray(array, size, n)
			if (copied != n)
				illegalState_!(
					errorString(xs) + " copied " + copied + " elements to " + this + " instead of full " + n + "."
				)
			size += n
			this
	}
	protected def initArraySize :Int = DefaultInitialSize

	/** Create the $coll containing elements `[0..size)` of `array`.
	  * The collection may use the array itself, sure that it won't be externally modified in the future.
	  */
	protected def result(array :Array[ArrayElem], size :Int) :To

	override def result() :To = {
		val res = result(array, size)
		array = ArrayFactory.empty
		size  = 0
		res
	}
	override def clear() :Unit = {
		array = ArrayFactory.empty
		size  = 0
	}
}




/** Base trait for general purpose builders overriding `addAll` with optimized implementations
  * for the most common collection types.
  */
trait BaseGrowable[-E] extends Growable[E] {
	override def addAll(elems :IterableOnce[E]) :this.type = elems match {
		case empty :Iterable[_] if !empty.isInstanceOf[View[_]] && empty.isEmpty =>
			this
		case list :collection.LinearSeq[E] =>
			var rest = list
			while (rest.nonEmpty) {
				addOne(rest.head)
				rest = rest.tail
			}
			this
		case ErasedArray.Slice(array :Array[E @unchecked], from, until) =>
			var i = from
			while (i < until) {
				addOne(array(i))
				i += 1
			}
			this
		case ApplyPreferred(seq) =>
			var i = seq.length
			while (i > 0) {
				i -= 1
				addOne(seq(i))
			}
			this
		case _ =>
			val i = elems.iterator
			while (i.hasNext)
				addOne(i.next())
			this
	}
}






/** A simple `Array[Any]`-based stack implementation.
  * A base class for builders everywhere, but also iterators over tree-like structures which need to keep track
  * of the path to the current leaf, possibilities are limitless!
  * @note Designed for trusted code, it does not perform bounds check when accessing the array.
  */
private class ArrayGrowable[E](private[this] var capacity :Int)
	extends ArrayIterableOnce[E] with Growable[E]
{
	def this() = this(-1)

	if (capacity <= 0)
		capacity = ArrayBuffer.DefaultInitialSize
	private[this] var stack = new Array[Any](capacity)
	private[this] var len = 0

	final override def knownSize :Int = len

	override def unsafeArray :Array[Any] = stack
	override def startIndex = 0


	protected final def apply(idx :Int) :E = stack(idx).asInstanceOf[E]
	protected final def pop() :E = { len -= 1; stack(len).asInstanceOf[E] }
	protected final def top :E = stack(len - 1).asInstanceOf[E]

	//todo: addAll
	override def addOne(elem :E) :this.type = {
		if (len == capacity) {
			capacity = math.max(math.min(MaxArraySize >> 1, capacity) << 1, ArrayBuffer.DefaultInitialSize)
			if (stack == null)
				stack = new Array[Any](capacity)
			else
				stack = Array.copyOf(stack, capacity)
		}
		stack(len) = elem
		len += 1
		this
	}

	override def clear() :Unit = {
		capacity = 0
		len      = 0
		stack    = null
	}

	override def iterator :Iterator[E] = new ArrayIterator[E](stack.asInstanceOf[Array[E]], 0, len, false)
	def toRefArray :RefArray[E] = stack.slice(0, len).asInstanceOf[RefArray[E]]
}
