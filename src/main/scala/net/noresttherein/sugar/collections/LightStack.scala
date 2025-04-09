package net.noresttherein.sugar.collections

import scala.annotation.nowarn
import scala.collection.{IterableFactory, StrictOptimizedIterableOps}
import scala.collection.mutable.{ArrayBuffer, Builder}

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayIterator, ArrayLike, ArrayLikeExtension, IRefArray, RefArray, ReverseArrayIterator, arraycopy}
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.LightStack.InitialSize
import net.noresttherein.sugar.collections.util.{elementsToCopy, errorString}
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.{MaxSizeReachedException, maxSize_!, noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.One




/** $factoryInfo
  * @define Coll `LightStack`
  * @define coll light stack
  */ //todo: LightBitSet.
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
					val res   = array.double
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
			addOne(0.asInstanceOf[A])

			override def result() :LightStack[A] = {
				val array = unsafeArray
				array(0) = knownSize
				clear()
				new LightStack(array)
			}
		}

	private[this] val prototype = new LightStack[Nothing](Array[Any](0))
}


/** $Coll is the lightest possible implementation of a growing buffer.
  * It is a value class backed by an `Array[Any]` with very simple inlineable methods, and an empty stack
  * is just a syntactic wrapper over an empty prototype, so you can pass it around and not even allocate the array
  * until you actually need it. As a value class, it cannot have variable fields, and thus its growing mechanism makes
  * it a weird combination of mutable and immutable interface.
  *   1. Methods named after those in `Growable`: `addOne`, `addAll`, `+=`, `++=`, do not reallocate the array
  *      and throw a [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]]
  *      if their maximum capacity is reached.
  *   1. Traditional stack methods - `push`, `pushAll` will reallocate the array as needed,
  *      returning a new stack instance. If the user wants to use the growing capacity of the stack,
  *      they ''must'' discard the stack on which the latter methods were called, replacing them with
  *      the returned $Coll.
  *
  * This class is designed as an internal tool for classes, allowing them to implement their methods
  * with minimal overhead. It extends [[net.noresttherein.sugar.collections.ArrayIterableOnce ArrayIterableOnce]]
  * (and, [[scala.collection.IterableOnce IterableOnce]]) for added interoperability,
  * but it is not meant to be exposed outside the method or class scope in which it was created. For this reason,
  * ''no additional bounds checks are performed'' over those done by the virtual machine. As a result,
  * operations can throw `NullPointerException`, or return `null` instances from `apply`
  * instead of throwing an `IndexOutOfBoundsException`.
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
		private val stack :Array[Any] //Not Iterable, because it does not extend Any
	) extends AnyVal with IterableOnce[E] with StrictOptimizedIterableOps[E, LightStack, LightStack[E]]
	     with ArraySlicingOps[E, LightStack, LightStack[E]] with Serializable
{
	@inline override def knownSize :Int = stack(0).asInstanceOf[Int]
	@inline override def size :Int = stack(0).asInstanceOf[Int]
	@inline def length :Int = stack(0).asInstanceOf[Int]

	/** The current array capacity. */
	@inline def cap :Int = stack.length - 1

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

	@inline override def last :E = stack(stack(0).asInstanceOf[Int]).asInstanceOf[E]
	@inline override def lastOption :Option[E] = stack(0).asInstanceOf[Int] match {
		case 0 => None
		case n => Some(stack(n).asInstanceOf[E])
	}
	def removeLast() :E = stack(0).asInstanceOf[Int] match {
		case 0 => noSuch_!("LightStack().removeLast()")
		case n => stack(0) = n - 1; stack(n).asInstanceOf[E]
	}


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

	/** Remove the top element of the stack, if non-empty, and return it. */
	def popOpt() :Opt[E] = {
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
	def push(elem :E) :LightStack[E] = {
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

	def pushAll(elems :IterableOnce[E]) :LightStack[E] = elems.knownSize match {
		case  0 => this
		case -1 =>
			val it = elems.iterator
			var size = stack(0).asInstanceOf[Int]
			var cap  = stack.length - 1
			var arr  = stack
			while (it.hasNext) {
				if (size == cap) {
					cap <<= 1
					arr = Array.copyOf(arr, cap)
				}
				size += 1
				arr(size) = it.next()
			}
			arr(0) = size
			new LightStack(arr)
		case  n if length + n <= stack.length - 1 =>
			val length = stack(0).asInstanceOf[Int]
			elems.toBasicOps.copyToArray(stack, length + 1, n)
			stack(0) = length + n
			this
		case  n =>
			val length = stack(0).asInstanceOf[Int]
			val copy = Array.copyOf(stack, math.max(stack.length << 1, length + n + 1))
			elems.toBasicOps.copyToArray(copy, length + 1, n)
			copy(0) = length + n
			new LightStack(copy)
	}

	def pushAll(elems :LightStack[E]) :LightStack[E] = {
		val thisSize = stack(0).asInstanceOf[Int]
		val thatSize = elems.stack(0).asInstanceOf[Int]
		val capacity = stack.length - 1
		val res =
			if (thisSize + thatSize <= capacity) {
				arraycopy(elems.stack, 1, stack, 1 + thisSize, thatSize)
				stack
			} else {
				val newSize = math.min(MaxArraySize >> 1, math.max(capacity + 1, thisSize + thatSize + 2 >> 1)) << 1
				Array.copyOfRanges(stack, 0, 1 + thisSize, elems.stack, 1, 1 + thatSize, newSize)
			}
		res(0) = thisSize + thatSize
		new LightStack(res)
	}

	/** Adds a single element to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array is full.")
	@inline def +=(elem :E) :this.type = addOne(elem)

	/** Adds a single element to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array is full.")
	def addOne(elem :E) :this.type = {
		val size = stack(0).asInstanceOf[Int]
		if (size == stack.length - 1)
			maxSize_!("LightStack|" + size + "|")
		stack(0) = size + 1
		stack(size + 1) = elem
		this
	}

	/** Adds multiple elements to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array becomes full.")
	@inline def ++=(elems :IterableOnce[E]) :this.type = addAll(elems)

	/** Adds multiple elements to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array becomes full.")
	def addAll(elems :IterableOnce[E]) :this.type = elems.knownSize match {
		case  0 => this
		case -1 =>
			val it = elems.iterator
			var size = stack(0).asInstanceOf[Int]
			val cap  = stack.length - 1
			while (it.hasNext) {
				if (size == cap)
					maxSize_!("LightStack|" + size + "|")
				size += 1
				stack(size) = it.next()
			}
			stack(0) = size
			this
		case  n if length + n <= stack.length - 1 =>
			val length = stack(0).asInstanceOf[Int]
			elems.toBasicOps.copyToArray(stack, length + 1, n)
			stack(0) = length + n
			this
		case  _ =>
			maxSize_!("LightStack|" + length + "|.addAll(" + errorString(elems) +")")
	}

	/** Adds multiple elements to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array becomes full.")
	@inline def ++=(elems :LightStack[E]) :this.type = addAll(elems)

	/** Adds multiple elements to the end (top) of the stack. */
	@throws[MaxSizeReachedException]("if the underlying array becomes full.")
	def addAll(elems :LightStack[E]) :this.type = {
		val thisSize = stack(0).asInstanceOf[Int]
		val thatSize = elems.stack(0).asInstanceOf[Int]
		val capacity = stack.length - 1
		if (capacity < thisSize + thatSize)
			maxSize_!("LightStack|" + thisSize + "|.addAll(" + errorString(elems) +")")
		arraycopy(elems.stack, 1, stack, 1 + thisSize, thatSize)
		stack(0) = thisSize + thatSize
		this
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
			ArrayLike.copy(stack.asInstanceOf[RefArray[U]], 1, xs, start, copied)
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
		new ReverseArrayIterator(stack, 0, size).asInstanceOf[Iterator[E]]

	def toIRefArray :IRefArray[E] =
		IRefArray.copyOfRange(stack.asInstanceOf[ArrayLike[E]], 1, stack(0).asInstanceOf[Int])

	def toRefArray :RefArray[E] =
		RefArray.copyOfRange(stack.asInstanceOf[ArrayLike[E]], 1, stack(0).asInstanceOf[Int])

	override def iterableFactory :IterableFactory[LightStack] = LightStack
	protected override def fromSpecific(coll :IterableOnce[E]) :LightStack[E] = LightStack.from(coll)
	protected override def newSpecificBuilder :Builder[E, LightStack[E]] = LightStack.newBuilder

	// overridden for efficiency, since we know CC[A] =:= C
	override def empty :LightStack[E] = LightStack.empty

	override def toString :String = mkString("LightStack(", ", ", ")")
}






/** $factoryInfo
  * @define Coll `LightQueue`
  * @define coll light queue
  */
@SerialVersionUID(Ver)
case object LightQueue extends IterableFactory[LightQueue] {
	override def from[A](source :IterableOnce[A]) :LightQueue[A] = source.knownSize match {
		case  0 => empty
		case -1 =>
			val queue = source.toBasicOps.foldLeft(Empty.queue) {
				case (array, elem) if array(0) == array.length - 2 =>
					val res       = array.double
					val size      = res(0).asInstanceOf[Int]
					res(0)        = size + 1
					res(size + 1) = elem
					res
				case (array, elem) =>
					val size        = array(0).asInstanceOf[Int]
					array(0)        = size + 1
					array(size + 1) = elem
					array
			}
			new LightQueue(queue)
		case  n =>
			val queue = new Array[Any](n + 2)
			queue(0) = n
			queue(1) = 2
			source.toBasicOps.copyToArray(queue, 2, Int.MaxValue)
			new LightQueue(queue)
	}


	override def newBuilder[A] :Builder[A, LightQueue[A]] =
		new ArrayGrowable[A]() with Builder[A, LightQueue[A]] {
			addOne(0.asInstanceOf[A])
			addOne(2.asInstanceOf[A])

			override def result() :LightQueue[A] = {
				val array = unsafeArray
				array(0) = knownSize
				clear()
				new LightQueue(array)
			}
		}

	override def empty[A] :LightQueue[A] = Empty.asInstanceOf[LightQueue[A]]
	@inline def of[E] :LightQueue[E] = Empty.asInstanceOf[LightQueue[E]]
	@inline def ofCapacity[E](capacity :Int) :LightQueue[E] = {
		val queue = new Array[Any](capacity + 2)
		queue(0) = 0
		queue(1) = 2
		new LightQueue(queue)
	}

	private val Empty = new LightQueue(Array[Any](0, 2))
}


/** $Coll is the lightest possible implementation of a circular queue.
  * It is a value class backed by an `Array[Any]` with very simple inlineable methods, and an empty queue
  * is just a syntactic wrapper over an empty prototype, so you can pass it around and not even allocate the array
  * until you actually need it. As a value class, it cannot have variable fields, and thus growing can be only
  * accomplished by creating a new $coll. Methods `push` and `pushAll` are therefore an unholy mix of mutable
  * and immutable interfaces: if there is still space in the underlying array, they return the same instance,
  * and only create a fresh copy when the array's capacity is reached. For this reason, the caller of these methods
  * ''must'' discard the instance on which they were called and use the returned queue from that point onward.
  *
  * A set of traditional `Buffer`/`Growable` methods is also provided, which always return this instance and
  * throw a [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]] if the array
  * becomes full instead. The queue is never shrunk as a result of popping the first element, but it can be requested
  * by the call to `shrink`.
  *
  * This class is designed as internal tool for classes, allowing them to implement their methods
  * with minimal overhead. It extends [[scala.collection.IterableOnce IterableOnce]] for added interoperability,
  * but using it through that interface will result in wrapping, negating the advantage of this class over
  * the standard library `Queue`. For efficiency, ''no additional bounds checks are performed'' over those done
  * by the virtual machine, and operations can throw `NullPointerException`, or return `null` instances from `apply`
  * * instead of throwing an `IndexOutOfBoundsException`. As the result, it should not be exposed outside
  * the owning method or class.
  *
  * @note Due to the semantics of value class equality, two instances may compare unequal
  *       even if they contain the same elements.
  * @note This class may prove less efficient than specialized implementations for large collections.
  * @define Coll `LightQueue`
  * @define coll light queue
  */
@SerialVersionUID(Ver)
class LightQueue[E] private[collections] (
	/** The backing array. The first element is an `Int` specifying the number of the elements in the queue,
	  * and the second element must be greater or equal `2` and is the index of the first element.
	  * Positions from `2` and above hold queue contents, wrapping at the end.
	  */
	private val queue :Array[Any] //Not Iterable, because it does not extend Any
) extends AnyVal with IterableOnce[E] with StrictOptimizedIterableOps[E, LightQueue, LightQueue[E]]
	with Serializable
{
	@inline override def knownSize :Int = queue(0).asInstanceOf[Int]
	@inline override def size :Int = queue(0).asInstanceOf[Int]
	@inline def length :Int = queue(0).asInstanceOf[Int]

	/** The current array capacity. */
	@inline def cap :Int = queue.length - 1

	@inline override def head :E =
		queue(queue(1).asInstanceOf[Int]).asInstanceOf[E]

	@inline override def headOption :Option[E] = queue(0).asInstanceOf[Int] match {
		case 0 => None
		case n => Some(queue(n).asInstanceOf[E])
	}

	@inline def first :E =
		queue(queue(1).asInstanceOf[Int]).asInstanceOf[E]

	@inline def firstOpt :Opt[E] = queue(0).asInstanceOf[Int] match {
		case 0 => None
		case n => One(queue(n).asInstanceOf[E])
	}

	def pop() :E = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		val limit  = queue.length
		val res    = queue(offset).asInstanceOf[E]
		queue(1)   = if (offset == limit - 1) 2 else offset + 1
		queue(0)   = length - 1
		res
	}
	def popOpt() :Opt[E] = queue(0).asInstanceOf[Int] match {
		case 0 => None
		case n =>
			val limit  = queue.length
			val offset = queue(1).asInstanceOf[Int]
			val res    = queue(offset).asInstanceOf[E]
			queue(1)   = if (offset == limit - 1) 2 else offset + 1
			queue(0)   = n - 1
			One(res)
	}

	def push(elem :E) :LightQueue[E] = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		var limit  = queue.length
		val res =
			if (length < limit - 2)
				queue
			else {
				val a = new Array[Any](limit << 1)
				if (offset <= limit - length)
					arraycopy(queue, offset, a, 2, length)
				else {
					arraycopy(queue, offset, a, 2, limit - offset)
					arraycopy(queue, 2, a, 2 + limit - offset, offset + length - limit)
					a(0) = length
					a(1) = 2
				}
				limit <<= 1
				a
			}
		val index = if (offset < limit - length) offset + length else 2 + offset + length - limit
		res(index) = elem
		res(0) = length + 1
		new LightQueue(res)
	}
	//todo: def pushAll(elems :IterableOnce[E]) :LightQueue[E] = ???
	//todo: def pushAll(elems :LightQueue[E]) :LightQueue[E] = ???

	def pushFront(elem :E) :LightQueue[E] = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		var limit  = queue.length
		val res =
			if (length < limit - 2)
				queue
			else {
				val a = new Array[Any](limit << 1) //todo: check for MaxArraySize
				if (offset <= limit - length)
					arraycopy(queue, offset, a, 3, length)
				else {
					arraycopy(queue, offset, a, 3, limit - offset)
					arraycopy(queue, 2, a, 3 + limit - offset, offset + length - limit)
					a(0) = length
					a(1) = 2
				}
				limit <<= 1
				a
			}
		val index = if (offset > 2) offset - 1 else limit - 1
		res(index) = elem
		res(0) = length + 2
		res(1) = index
		new LightQueue(res)
	}

	@inline def +=(elem :E) :this.type = addOne(elem)

	def addOne(elem :E) :this.type = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		val limit  = queue.length
		if (length == limit - 2)
			maxSize_!("LightQueue|" + length + "|.addOne")
		if (offset < limit - length)
			queue(offset + length) = elem
		else
			queue(2 + offset + length - limit) = elem
		queue(0) = length + 1
		this
	}

	@inline def +=:(elem :E) :this.type = prependOne(elem)

	def prependOne(elem :E) :this.type = {
		val length = queue(0).asInstanceOf[Int]
		var offset = queue(1).asInstanceOf[Int]
		val limit  = queue.length
		if (length == limit - 2)
			maxSize_!("LightQueue|" + length + "|.prependOne")
		offset = if (offset > 2) offset - 1 else limit - 1
		queue(0) = length + 1
		queue(1) = offset
		this
	}

	//todo: addAll(elems :IterableOnce[E]) :this.type = ???
	//todo: addAll(elems :LightQueue[E]) :this.type = ???

	@nowarn("cat=deprecation") @inline protected override def coll :LightQueue[E] = this
	@nowarn("cat=deprecation") override def toIterable :Iterable[E] = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		if (offset + length <= queue.length)
			RefArray.Slice(queue.asInstanceOf[RefArray[E]], offset, length)
		else
			RefArray.Wrapped(toRefArray)
	}

	override def iterator :Iterator[E] = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		new AbstractCyclicIterator[E](offset, length, queue.length) {
			override def head = queue(index).asInstanceOf[E]
			override def rangeStart = 2
		}
	}

	def reverseIterator :Iterator[E] = {
		val offset = queue(1).asInstanceOf[Int]
		val length = queue(0).asInstanceOf[Int]
		val limit  = queue.length - 2
		new AbstractReverseCyclicIterator[E](2 + (offset + length - 2) % limit, length, 2, limit + 2) {
			override def head = queue(index).asInstanceOf[E]
		}
	}

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
		val length = queue(0).asInstanceOf[Int]
		val offset = queue(1).asInstanceOf[Int]
		val limit  = queue.length
		val copied = elementsToCopy(length, xs, start, len)
		if (offset <= limit - copied)
			Array.copy(queue, offset, xs, start, copied)
		else {
			val suffix = limit - offset
			Array.copy(queue, offset, xs, start, suffix)
			Array.copy(queue, 2, xs, start + suffix, copied - suffix)
		}
		copied
	}

	def toIRefArray :IRefArray[E] = toRefArray.asInstanceOf[IRefArray[E]]

	def toRefArray :RefArray[E] = {
		val length   = queue(0).asInstanceOf[Int]
		val offset   = queue(1).asInstanceOf[Int]
		val capacity = queue.length
		if (length == 0)
			RefArray.empty
		else if (offset <= capacity - length)
			RefArray.copyOfRange(queue.asInstanceOf[RefArray[E]], offset, offset + length)
		else {
			val res = RefArray.ofDim[E](length)
			arraycopy(queue, offset, res, 0, capacity - offset)
			arraycopy(queue, 2, res, capacity - offset, offset + length - capacity)
			res
		}
	}

	override def iterableFactory :IterableFactory[LightQueue] = LightQueue
	protected override def fromSpecific(coll :IterableOnce[E]) :LightQueue[E] = LightQueue.from(coll)
	protected override def newSpecificBuilder :Builder[E, LightQueue[E]] = LightQueue.newBuilder

	// overridden for efficiency, since we know CC[A] =:= C
	override def empty :LightQueue[E] = LightQueue.empty

	override def toString :String = mkString("LightQueue(", ", ", ")")
}
