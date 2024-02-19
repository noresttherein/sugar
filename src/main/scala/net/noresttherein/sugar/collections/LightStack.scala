package net.noresttherein.sugar.collections

import scala.annotation.nowarn
import scala.collection.{IterableFactory, IterableOps}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayIterator, ArrayLike, IRefArray, RefArray, ReverseArrayIterator}
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.LightStack.InitialSize
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.One




/** $factoryInfo
  * @define Coll `LightStack`
  * @define coll light stack
  */
@SerialVersionUID(Ver)
case object LightStack extends IterableFactory[LightStack] {
	final val InitialSize = ArrayBuffer.DefaultInitialSize

	/** A shorted alternative for `empty`. */
	@inline def of[E] :LightStack[E] = prototype.asInstanceOf[LightStack[E]]
	@inline override def empty[E] :LightStack[E] = prototype.asInstanceOf[LightStack[E]]
	@inline def ofCapacity[E](capacity :Int) :LightStack[E] = {
		val stack = new Array[Any](capacity + 1)
		stack(0) = 0
		new LightStack(stack)
	}

	override def from[A](source :IterableOnce[A]) :LightStack[A] = source.knownSize match {
		case  0 => empty
		case -1 =>
			val stack = source.toBasicOps.foldLeft(prototype.stack) {
				case (array, elem) if array(0) == array.length - 1 =>
					val res   = Array.copyOf(array, math.min(MaxArraySize >> 1, array.length) << 1)
					val size  = res(0).asInstanceOf[Int]
					res(0)    = size + 1
					res(size) = elem
					res
				case (array, elem) =>
					val size    = array(0).asInstanceOf[Int]
					array(0)    = size + 1
					array(size) = elem
					array
			}
			new LightStack(stack)
		case  n =>
			val stack = new Array[Any](n + 1)
			stack(0) = n
			source.toBasicOps.copyToArray(stack, 1, Int.MaxValue)
			new LightStack(stack)
	}

	override def newBuilder[A] :Builder[A, LightStack[A]] =
		new ArrayGrowable[A]() with Builder[A, LightStack[A]] {
			override def result() :LightStack[A] = {
				val array = Array.copyOfRange(unsafeArray, 0, knownSize, 1, knownSize)
				clear()
				new LightStack(array)
			}
		}

	private[this] val prototype = new LightStack[Nothing](Array[Any](0))
}


/** $Coll is the lightest possible implementation of a growing buffer.
  * It is a value class backed by an `Array[Any]` with very simple inlinable methods, and an empty stack
  * is just a syntactic wrapper over an empty prototype, so you can pass it around and not even allocate the array
  * until you actually need it. As a value class, it cannot have variable fields, and thus its growing mechanism makes
  * it a weird combination of mutable and immutable interface. A mutable `addOne`/`+=` method would be unable
  * to reallocate - or even initially allocate - the array in a way which would change the instance
  * for which it was called, but, on the other hand, an immutable `:+` still needs to modify the state
  * of the latter, instead of returning an independent instance, if the array is not full. For this reason,
  * neither set of operations is implemented, but rather the class provides the methods after the standard
  * stack operation names: `push`, `pop`, `top`. In case of `push`, the returned stack ''must'' be used
  * instead of the instance for which it was called, as the latter is left in an undefined state.
  *
  * This class is designed as internal tool for classes, allowing them to implement their methods
  * with minimal overhead. It extends [[net.noresttherein.sugar.collections.ArrayIterableOnce ArrayIterableOnce]]
  * (and, through it, [[scala.collection.IterableOnce IterableOnce]]) for added interoperability,
  * but it is not meant to be exposed method or class scope in which it was created. For this reason,
  * ''no additional bounds checks are performed'' over those done by the virtual machine. As a result,
  * operations can throw `NullPointerException`, or return `null` instances from `apply`.
  *
  * @note Due to the semantics of value class equality, two instances may compare unequal
  *       even if they contain the same elements.
  * @note This class may prove less efficient than specialized implementations for large collections.
  * @define Coll `LightStack`
  * @define coll light stack
  */
@SerialVersionUID(Ver)
class LightStack[E] private[collections] (
		/** The backing array. The first element is an `Int` specifying the stack size. */
		private val stack :Array[Any]
	) extends AnyVal with IterableOnce[E] with IterableOps[E, LightStack, LightStack[E]]
	     with ArraySliceOps[E, LightStack, LightStack[E]] with Serializable
{
	@inline override def knownSize :Int = stack(0).asInstanceOf[Int]
	@inline override def size :Int = stack(0).asInstanceOf[Int]
	@inline def length :Int = stack(0).asInstanceOf[Int]
	private[sugar] override def startIndex = 1
	private[sugar] override def unsafeArray :Array[Any] = stack

	@inline override def isEmpty :Boolean = stack(0).asInstanceOf[Int] == 0

	/** The `i`-th element on the stack, counting from the bottom.
	  * @note behaviour if `i >= size` is undefined.
	  *       The method may throw an exception, return `null` or a stale value.
	  */
	@inline def apply(i :Int) :E = stack(i + 1).asInstanceOf[E]

	/** Sets the `i`-th element on the stack, counting from the bottom, to the specified value.
	  * @note behaviour if `i >= size` is undefined.
	  */
	@inline def update(i :Int, elem :E) :Unit = stack(i + 1) = elem

	/** The most recently [[net.noresttherein.sugar.collections.LightStack.push pushed]] element on the stack.
	  * @note behaviour when called on an empty stack is undefined; the method may throw `NullPointerException`,
	  *       `ClassCastException`, or return a value not being an instance of `E` at all.
	  */
	@inline def top :E = stack(stack(0).asInstanceOf[Int]).asInstanceOf[E]

	/** The most recently [[net.noresttherein.sugar.collections.LightStack.push pushed]] element or `None`,
	  * if the stack is empty.
	  */
	def topOpt :Opt[E] = stack(0).asInstanceOf[Int] match {
		case 0 => None
		case n => One(stack(n).asInstanceOf[E])
	}

	/** Removes the top element from the stack and returns it. */
	@inline def pop() :E = {
		val size = stack(0).asInstanceOf[Int]
		stack(0) = size - 1
		//We could set stack(size) to null, but it isn't expected to be of importance on a shortly lived object.
		stack(size).asInstanceOf[E]
	}

	/** Remove the top element of the stack, if non empty, and return it. */
	@inline def popOpt() :Opt[E] = {
		val size = stack(0).asInstanceOf[Int]
		if (size == 0) None
		else {
			stack(0) = size - 1
			One(stack(size).asInstanceOf[E])
		}
	}

	/** Puts a new element on top of the stack, growing the underlying array if needed, and returns the modified stack.
	  * Leaves the instance in an undefined state: the application must use the returned stack from this point forward.
	  */
	@inline def push(elem :E) :LightStack[E] = {
		val size = stack(0).asInstanceOf[Int]
		val array =
			if (size < stack.length - 1)
				stack
			else
				Array.copyOf(stack, math.max(InitialSize, math.min(MaxArraySize >> 1, size + 1) << 1))
		array(0) = size + 1
		array(size + 1) = elem
		new LightStack(array)
	}

	/** Resets the size of the stack to zero.
	  * @note This does not unreference current contents of the buffer, or shrinks the array.
	  *       If you wish for a behaviour similar to `Buffer.`[[scala.collection.mutable.Buffer.clear clear]]`()`,
	  *       simply use a fresh, empty instance in its place.
	  */
	@inline def clear() :Unit = stack(0) = 0

	override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int = {
		val size = if (stack == null) 0 else stack(0).asInstanceOf[Int]
		if (len <= 0 | size == 0 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(start, size)
		else {
			val copied = math.min(math.min(xs.length - start, len), size)
			ArrayLike.copy(stack, 1, xs, start, copied)
			copied
		}
	}

	protected override def clippedSlice(from :Int, until :Int) :LightStack[E] = {
		val res = stack.slice(from, until + 1)
		res(0)  = until - from
		new LightStack(res)
	}

	@nowarn("cat=deprecation") @inline override def coll :LightStack[E] = this
	@nowarn("cat=deprecation") override def toIterable :Iterable[E] =
		ArraySlice.slice(stack, 1, size).asInstanceOf[Iterable[E]]

	@inline override def iterator :Iterator[E] =
		new ArrayIterator(stack, 1, 1 + size, false).asInstanceOf[Iterator[E]]

	@inline def reverseIterator   :Iterator[E] =
		new ReverseArrayIterator(stack, 1, 1 + size).asInstanceOf[Iterator[E]]

	def toIRefArray :IRefArray[E] =
		if (stack == null) IRefArray.empty
		else IRefArray.copyOfRange(stack.asInstanceOf[ArrayLike[E]], 1, stack(0).asInstanceOf[Int])

	def toRefArray :RefArray[E] =
		if (stack == null) RefArray.empty
		else RefArray.copyOfRange(stack.asInstanceOf[ArrayLike[E]], 1, stack(0).asInstanceOf[Int])

	override def iterableFactory :IterableFactory[LightStack] = LightStack
	protected override def fromSpecific(coll :IterableOnce[E]) :LightStack[E] = iterableFactory.from(coll)
	protected override def newSpecificBuilder :Builder[E, LightStack[E]] = iterableFactory.newBuilder

	// overridden for efficiency, since we know CC[A] =:= C
	override def empty :LightStack[E] = iterableFactory.empty

	override def toString :String = mkString("LightStack(", ", ", ")")
}
