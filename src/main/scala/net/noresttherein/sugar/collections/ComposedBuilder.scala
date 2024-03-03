package net.noresttherein.sugar.collections

import scala.collection.IterableOps
import scala.collection.mutable.{ArrayBuffer, Builder, Growable, ImmutableBuilder, ReusableBuilder}

import net.noresttherein.sugar.arrays.{ArrayIterator, RefArray}
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.extensions.IterableExtension




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


/*
trait BuilderFromBytes[To] extends Builder[Byte, To] {
	override def addOne(elem :Byte) :this.type
}
trait BuilderFromChars[To] extends Builder[Char, To] {
	override def addOne(elem :Char) :this.type
}
trait BuilderFromShorts[To] extends Builder[Short, To] {
	override def addOne(elem :Short) :this.type
}
trait BuilderFromInts[To] extends Builder[Int, To] {
	override def addOne(elem :Int) :this.type
}
trait BuilderFromLongs[To] extends Builder[Long, To] {
	override def addOne(elem :Long) :this.type
}
trait BuilderFromFloats[To] extends Builder[Float, To] {
	override def addOne(elem :Float) :this.type
}
trait BuilderFromDoubles[To] extends Builder[Double, To] {
	override def addOne(elem :Double) :this.type
}
trait BuilderFromBooleans[To] extends Builder[Boolean, To] {
	override def addOne(elem :Boolean) :this.type
}
*/


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
