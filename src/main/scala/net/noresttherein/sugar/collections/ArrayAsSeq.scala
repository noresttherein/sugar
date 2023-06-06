package net.noresttherein.sugar.collections

import java.lang.System.arraycopy
import java.util.Arrays

import scala.Array.{emptyBooleanArray, emptyByteArray, emptyCharArray, emptyDoubleArray, emptyFloatArray, emptyIntArray, emptyLongArray, emptyObjectArray, emptyShortArray}
import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.Stepper.EfficientSplit
import scala.collection.{ClassTagIterableFactory, IterableFactory, SeqFactory, Stepper, StepperShape, View, mutable}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.{ArrayBuilder, Builder, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.??!
import net.noresttherein.sugar.JavaTypes.JIterator
import net.noresttherein.sugar.extensions.{castTypeParamMethods, castingMethods, classNameMethods}
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf

//extension methods
import extensions._




/** A collection backed by an array slice. Extended by the 'proper' `PassedArray` and various `ArrayLike` adapters,
  *  but used instead of the latter in pattern matching to decouple code from the `PassedArray` type itself.
  */
private[collections] trait AbstractArraySlice[+E] extends IterableOnce[E] {
	private[collections] def unsafeArray :Array[_]
	private[collections] def startIndex :Int
	private[collections] def isImmutable :Boolean = false
}




/** A window over a range of indices in an `Array`. There is no guarantee that the component type of the array
  * is actually `classOf[A]` - it might be boxed (for value types), a super type (or a subtype) of `A`, or both.
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
private[collections] class ArraySliceSeq[+A] protected(whole :Array[A], offset :Int, override val length :Int)
	extends collection.AbstractSeq[A] with collection.IndexedSeq[A]
		with SugaredIterable[A] with AbstractArraySlice[A] with Serializable
{
	def this(whole :Array[A]) = this (whole, 0, whole.length)

	private[collections] override def unsafeArray :Array[_] = whole
	private[collections] override def startIndex  :Int = offset

	override def apply(i :Int) :A =
		if (i < 0 || i >= length)
			throw new IndexOutOfBoundsException(i.toString + " out of " + length)
		else
			whole(offset + i)

	override def iterator :Iterator[A] = ArrayIterator(whole, offset, offset + length)
	override def reverseIterator :Iterator[A] = ArrayIterator.reversed(whole, length + offset, offset)
	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
		JavaIterator.slice(whole, offset, offset + length)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[A, S]) :S with EfficientSplit =
		IndexedSeqStepper(whole, offset, offset + length)

	override def empty :ArraySliceSeq[A] = ArraySliceSeq.empty[A]
	override def take(n :Int) :ArraySliceSeq[A] = slice(0, n)
	override def drop(n :Int) :ArraySliceSeq[A] = slice(n, length)
	override def takeRight(n :Int) :ArraySliceSeq[A] = if (n <= 0) empty else slice(length - math.max(n, 0), length)
	override def dropRight(n :Int) :ArraySliceSeq[A] = if (n <= 0) this else slice(0, length - math.max(n, 0))

	override def slice(from :Int, until :Int) :ArraySliceSeq[A] =
		if (until < from | until <= 0 || from >= length) empty
		else if (from <= 0 && until >= length) this
		else if (from <= 0) new ArraySliceSeq(whole, offset, until)
		else new ArraySliceSeq(whole, offset + from, length - from)

	private def writeReplace = new Serializable {
		private[this] val data = whole.slice(offset, offset + length)
		private def readResolve = new ArraySliceSeq(data)
	}

	protected override def className :String = "ArraySliceSeq"
}




/** $factoryInfo
  * @define Coll `ArraySlice`
  * @define coll array slice
  */
@SerialVersionUID(Ver)
private[collections] case object ArraySliceSeq extends SeqFactory[ArraySliceSeq] {

	def apply[A](array :Array[A], from :Int, until :Int) :ArraySliceSeq[A] = {
		val len = array.length
		if (from >= len) new ArraySliceSeq(array, len, len)
		else if (until <= 0) new ArraySliceSeq(array, 0, 0)
		else if (until <= from) new ArraySliceSeq(array, from, from)
		else if (from <= 0 && until >= len) new ArraySliceSeq(array, 0, len)
		else if (from <= 0) new ArraySliceSeq(array, 0, until)
		else if (until >= len) new ArraySliceSeq(array, from, len - from)
		else new ArraySliceSeq(array, from, until - from)
	}

	override def from[A](source :IterableOnce[A]) :ArraySliceSeq[A] = source match {
		case it :View[A]                                   => from(it.iterator)
		case it :Iterable[_] if it.isEmpty                 => Empty.asInstanceOf[ArraySliceSeq[A]]
//		case it :ArraySliceSeq[A]                          => it
//		case it :ArraySeq[A]                               => new ArraySliceSeq(it.unsafeArray.asInstanceOf[Array[A]])
//		case it :AbstractPassedArray[A]                    => new ArraySliceSeq(it.unsafeArray, it.startIndex, it.length)
		case it :Iterator[_] if !it.hasNext                => Empty.asInstanceOf[ArraySliceSeq[A]]
		case ErasedArray.Wrapped.Slice(array, from, until) => new ArraySliceSeq(array.slice(from, until)).castParam[A]
		case it                                            => (newBuilder[A] ++= it).result()
	}

	override def newBuilder[A] :Builder[A, ArraySliceSeq[A]] =
		ErasedArray.newBuilder[A] mapResult (new ArraySliceSeq(_))

	override def empty[A] :ArraySliceSeq[A] = Empty.asInstanceOf[ArraySliceSeq[A]]

	private[this] val Empty = new ArraySliceSeq(Array.emptyObjectArray)


	private[ArraySliceSeq] sealed trait BaseImmutable[+E] extends ArraySliceSeq[E] {
		override def slice(from :Int, until :Int) :Immutable[E] = ??!
	}

	/** A view of a slice of an immutable array. This class duplicates the functionality
	  * of [[net.noresttherein.sugar.collections.PassedArray PassedArray]] However, if we separate
	  * the new collection library from extension methods in this package, the latter may lose access to it.
	  * This class - or rather, its factory object - is used
	  * by [[net.noresttherein.sugar.collections.IRefArray.Wrapped.Slice IRefArray.Wrapped.Slice]] as a default
	  * if `PassedArray` is not on the classpath.
	  *
	  * Note that while it nominally wraps an `IArray[A]`, it might be actually `IRefArray[A]`, or any other array
	  * type during execution. The only guarantees are that all its elements are subtypes of `A`
	  * (once accounted for transparent boxing and unboxing), and that only nominally immutable objects retain
	  * references to it. The As always, take caution when attempting to use
	  * [[net.noresttherein.sugar.collections.ArraySliceSeq.unsafeArray unsafeArray]] as an `IArray[A]`.
	  */
	class Immutable[+A] private (array :IArray[A], from :Int, until :Int)
		extends ArraySliceSeq[A](array.asInstanceOf[Array[A]], from, until) with BaseImmutable[A] with IndexedSeq[A]
	{
		override def isImmutable = true
		private def this(array :IArray[A]) = this(array, 0, array.length)

		override def empty :Immutable[A] = Immutable.empty[A]
		override def take(n :Int) :Immutable[A] = slice(0, n)
		override def drop(n :Int) :Immutable[A] = slice(n, length)
		override def takeRight(n :Int) :Immutable[A] = if (n <= 0) empty else slice(length - math.max(n, 0), length)
		override def dropRight(n :Int) :Immutable[A] = if (n <= 0) this else slice(0, length - math.max(n, 0))

		override def slice(from :Int, until :Int) :Immutable[A] =
			if (until < from | until <= 0 || from >= length) Immutable.empty
			else if (from <= 0 && until >= length) this
			else if (from <= 0) new Immutable(array, this.from, until)
			else new Immutable(array, this.from + from, length - from)

		protected override def applyPreferredMaxLength :Int = Int.MaxValue
	}

	/** $factoryInfo
	  * @define Coll `ArraySlice.Immutable`
	  * @define coll immutable array slice
	  */
	object Immutable extends SeqFactory[Immutable] {

		def apply[A](array :IArray[A], from :Int, until :Int) :Immutable[A] = {
			val len = array.length
			if (from >= len) new Immutable(array, len, len)
			else if (until <= 0) new Immutable(array, 0, 0)
			else if (until <= from) new Immutable(array, from, from)
			else if (from <= 0 && until >= len) new Immutable(array, 0, len)
			else if (from <= 0) new Immutable(array, 0, until)
			else if (until >= len) new Immutable(array, from, len - from)
			else new Immutable(array, from, until - from)
		}

		override def from[A](source :IterableOnce[A]) :Immutable[A] = source match {
			case it :View[A]                    => from(it.iterator)
			case it :Iterable[_] if it.isEmpty  => Empty.asInstanceOf[Immutable[A]]
			case it :Immutable[A]               => it
			case it :ArraySeq[A]                => new Immutable(it.unsafeArray.asInstanceOf[IArray[A]])
	//		case it :AbstractPassedArray[A]     => new ArraySliceSeq(it.unsafeArray, it.startIndex, it.length)
			case it :Iterator[_] if !it.hasNext => Empty.asInstanceOf[Immutable[A]]
			case it                             => (newBuilder[A] ++= it).result()
		}

		override def newBuilder[A] :Builder[A, Immutable[A]] =
			ErasedArray.newBuilder[A] mapResult { (array :Array[A]) => new Immutable(array.asInstanceOf[IArray[A]]) }

		override def empty[A] :Immutable[A] = Empty.asInstanceOf[Immutable[A]]

		private[this] val Empty = new Immutable(IArray.emptyObjectIArray)
	}
}






@SerialVersionUID(Ver)
private abstract class AbstractArrayAsSeq[E, CC[_], C](arr :Array[E])
	extends IterableOnce[E] with collection.IndexedSeqOps[E, CC, C]
	   with AbstractArraySlice[E] with SugaredIterableOps[E, CC, C]
{
	override def unsafeArray :Array[_] = arr
	override def startIndex :Int = 0
	override def isImmutable :Boolean = false

	override def length :Int = arr.length
	override def apply(i :Int) :E = arr(i)
	override def iterator :Iterator[E] = ArrayIterator(array)
//	override def empty = iterableFactory.empty

	override def toSeq :Seq[E] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped(arr.toIRefArray)


	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[B >: E](xs :Array[B], start :Int, from :Int, until :Int) =
		if (until <= from | until <= 0 | arr.length >= 0 & from >= arr.length || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				arr.className + "<" + arr.length + ">.copyRangeToArray(" + xs.className + "<" + xs.length + ">, " +
					start + ", " + from + ", " + until + ")"
			)
		else {
			val from0 = math.max(from, 0)
			val copied = math.min(xs.length - start, math.min(until, arr.length) - from0)
			ArrayLike.copy(arr, from, xs, start, copied)
			copied
		}

	override def equals(that :Any) :Boolean = that match {
		case other :AbstractArrayAsSeq[_, _, _] => arr sameElements other.array
		case it :IterableOnce[_]   => mutable.ArraySeq.make(arr) == it
		case _                     => false
	}
	@inline final def array :Array[E] = arr

	override def hashCode :Int = mutable.ArraySeq.make(arr).hashCode

	protected def className :String = iterableFactory.toString
	override def toString :String = mutable.ArraySeq.make(arr).mkString(className + "(", ", ", ")")
}







/** A non-sticky adapter of an `Array[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, Array, Array[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class, with the exception
  * of those which potentially change the element type, which return an erased
  * [[net.noresttherein.sugar.collections.RefArray RefArray]].
  *
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * is that the latter's methods return `Seq`, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class ArrayAsSeq[E](override val coll :Array[E])
	extends AbstractArrayAsSeq[E, RefArray, Array[E]](coll) with mutable.IndexedSeqOps[E, RefArray, Array[E]]
{
	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem

	override def removed(index :Int) :Array[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :Array[E] = coll.removed(from, until)

	override def empty :Array[E] =
		ArrayAsSeq.empty(coll.getClass.getComponentType.castParam[E])

	protected override def fromSpecific(coll :IterableOnce[E]) :Array[E] =
		ArrayAsSeq.from(coll)

	protected override def newSpecificBuilder :Builder[E, Array[E]] =
		ArrayAsSeq.newBuilder[E](coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

//	override def iterableFactory :IterableFactory[Array] = ErasedArray
	override def iterableFactory :IterableFactory[RefArray] = RefArray

	private implicit def classTag :ClassTag[E] = ClassTag(coll.getClass.getComponentType)
	override def className = "Array"
}




@SerialVersionUID(Ver)
private object ArrayAsSeq extends ClassTagIterableFactory[Array] {

	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
		case elems :Iterable[E] if elems.knownSize == 0 => Array.empty[E]
		case iter  :Iterator[E] if !iter.hasNext => Array.empty[E]
		case elems :ArrayAsSeq[E] if elems.coll.getClass.getComponentType == classTag[E].runtimeClass => elems.coll
		case elems :Iterable[E] => elems.toArray[E]
		case _ => it.iterator.toArray[E]
	}

	//Array.copyAs resorts to slowcopy if not classOf[E] >:> array.getClass.getComponentType.
	// We instead always default to System.arraycopy if both element types are reference types.
	def copyAs[E :ClassTag](array :ArrayLike[E], start :Int, newLength :Int) :Array[E] =
		if (start < 0)
			throw new IndexOutOfBoundsException(
				"ArrayAsSeq.copyAs[" + fullNameOf[E] + "](" + array.className + "<" + array.length + ">, " +
					start + ", " + newLength + ")"
			)
		else if (newLength == 0)
			empty[E]
		else {
			val res = new Array[E](newLength)
			if (start < newLength)
			ArrayLike.copy(array, 0, res, start, math.min(newLength - start, array.length))
			res
		}

	def copyAs[E](array :ArrayLike[E], newLength :Int)(implicit classTag :ClassTag[E]) :Array[E] =
		if (newLength == 0)
			empty[E]
		else {
			val elemClass = classTag.runtimeClass
			if (elemClass isAssignableFrom array.getClass.getComponentType)
				if (elemClass.isPrimitive)
					Array.copyOf(array.castFrom[ArrayLike[E], Array[E]], newLength)
				else
					Arrays.copyOf(array.castFrom[ArrayLike[E], Array[AnyRef]], newLength, elemClass.castParam[Array[AnyRef]])
					      .castFrom[Array[AnyRef], Array[E]]
			else {
				val res = new Array[E](newLength)
				ArrayLike.copy(array, 0, res, 0, newLength)
				res
			}
		}

	@inline def copyAs[E :ClassTag](array :ArrayLike[E]) :Array[E] =
		copyAs(array, array.asInstanceOf[Array[_]].length)

	@inline def copyOf[E](array :Array[E]) :Array[E] =
		if (array.length == 0) empty(array.getClass.getComponentType.castParam[E])
		else Array.copyOf(array, array.length)

	@inline def copyOf[E](array :Array[E], newLength :Int) :Array[E] =
		if (newLength == 0) empty(array.getClass.getComponentType.castParam[E])
		else Array.copyOf(array, array.length)

	def copyOf[E](array :Array[E], start :Int, newLength :Int) :Array[E] =
		if (start < 0)
			throw new IndexOutOfBoundsException(
				"ArrayAsSeq.copyOf(" + array.className + "<" + array.length + ">, " + start + ", " + newLength + ")"
			)
		else if (newLength == 0)
			empty(array.getClass.getComponentType.castParam[E])
		else {
			val res = Array.make(array.getClass.getComponentType.castParam[E], newLength)
			if (start < newLength)
				ArrayLike.copy(array, 0, res, start, math.min(newLength - start, array.length))
			res
		}

	def empty[A](elementType :Class[A]) :Array[A] = {
		if (classOf[AnyRef] isAssignableFrom elementType) emptyObjectArray
		else if (elementType == classOf[Int]) emptyIntArray
		else if (elementType == classOf[Long]) emptyLongArray
		else if (elementType == classOf[Double]) emptyDoubleArray
		else if (elementType == classOf[Byte]) emptyByteArray
		else if (elementType == classOf[Char]) emptyCharArray
		else if (elementType == classOf[Float]) emptyFloatArray
		else if (elementType == classOf[Short]) emptyShortArray
		else if (elementType == classOf[Boolean]) emptyBooleanArray
		else emptyObjectArray
	}.asInstanceOf[Array[A]]

	override def empty[A :ClassTag] :Array[A] = empty(classTag[A].runtimeClass.asInstanceOf[Class[A]])

	override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder[A] //new ArrayBuilder[A]

//	def newBuilder[A](init :Array[A]) :Builder[A, Array[A]] =
//		new ArrayBuilder(init)

	def newBuilder[A](elementType :Class[A]) :Builder[A, Array[A]] =
		if (elementType == classOf[AnyRef])
			new ArrayBuilder.ofRef[AnyRef].asInstanceOf[Builder[A, Array[A]]]
		else if (classOf[AnyRef] isAssignableFrom elementType)
			new ArrayAsSeq.ArrayBuilder(elementType)
		else {
			if (elementType == classOf[Int]) new ArrayBuilder.ofInt
			else if (elementType == classOf[Long]) new ArrayBuilder.ofLong
			else if (elementType == classOf[Double]) new ArrayBuilder.ofDouble
			else if (elementType == classOf[Byte]) new ArrayBuilder.ofByte
			else if (elementType == classOf[Char]) new ArrayBuilder.ofChar
			else if (elementType == classOf[Float]) new ArrayBuilder.ofFloat
			else if (elementType == classOf[Short]) new ArrayBuilder.ofShort
			else if (elementType == classOf[Boolean]) new ArrayBuilder.ofBoolean
			else if (elementType == classOf[Unit]) new ArrayBuilder.ofUnit
			else ArrayBuilder.make(ClassTag(elementType))
		}.asInstanceOf[Builder[A, Array[A]]]

	private val InitialBuilderSize = 16

	/** Our own version of a builder for  */
	private class ArrayBuilder[A](private[this] var init :Array[A], elemType :Class[A])
		extends ReusableBuilder[A, Array[A]]
	{
		def this(elemType :Class[A]) = this(null, elemType)
		def this()(implicit tag :ClassTag[A]) = this(tag.runtimeClass.castParam[A])
		def this(buffer :Array[A]) = this(buffer, buffer.getClass.getComponentType.castParam[A])

		private[this] var buffer = init
		private[this] var size :Int = if (buffer == null) 0 else buffer.length

		override def sizeHint(size :Int) :Unit =
			if (size > 0) {
				if (buffer == null)
					buffer = Array.make(elemType, size)
				else if (buffer.length * 2 < size)
					buffer = Array.copyOf(buffer, size)
			}

		override def addAll(xs :IterableOnce[A]) :this.type =
			xs match {
				case it :Iterable[A] if it.isEmpty => this
				case it :Iterator[A] if !it.hasNext => this
				case it :Iterable[A] => it.knownSize match {
					case unknown if unknown < 0 => super.addAll(xs)
					case extra if buffer == null =>
						buffer = Array.make(elemType, extra)
						it.copyToArray(buffer)
						size = extra
						this
					case extra if size + extra <= buffer.length =>
						it.copyToArray(buffer, size)
						size += extra
						this
					case extra =>
						buffer = Array.copyOf(buffer, buffer.length * 2 max size + extra)
						this
				}
				case _ =>
					val it = xs.iterator
					it.knownSize match {
						case unknown if unknown < 0 => super.addAll(xs)
						case extra if buffer == null =>
							buffer = Array.make(elemType, extra)
							it.copyToArray(buffer)
							size = extra
						case extra if size + extra <= buffer.length =>
							it.copyToArray(buffer, size)
							size += extra
						case extra =>
							buffer = Array.copyOf(buffer, buffer.length * 2 max size + extra)
					}
					this
			}

		override def addOne(elem :A) = {
			if (buffer == null)
				buffer = Array.make(elemType, InitialBuilderSize)
			else if (size == buffer.length)
				buffer = Array.copyOf(buffer, buffer.length * 2)
			buffer(size) = elem
			size += 1
			this
		}

		override def clear() :Unit = {
			init = null
			buffer = null
			size = 0
		}

		override def result() =
			if (size == 0)
				ArrayAsSeq.empty(elemType)
			else if (size == buffer.length && (buffer ne init))
				buffer
			else {
				val res = Array.copyOf(buffer, size)
				init = null
				buffer = null
				size = 0
				res
			}
	}
}






/** A non-sticky adapter of an `IArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, IArray, IArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class, with the exception
  * of those which potentially change the element type, which return an erased
  * [[net.noresttherein.sugar.collections.IRefArray IRefArray]].
  *
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is neither an `IterableOnce`, nor `IndexedSeqOps`, and thus not marked as immutable.
  * On the other hand, explicitly created [[scala.collection.immutable.ArraySeq ArraySeq]]
  * is that the latter's methods return `Seq`, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `IArray`s.
  */
@SerialVersionUID(Ver)
private final class IArrayAsSeq[E](override val coll :IArray[E])
	extends AbstractArrayAsSeq[E, IRefArray, IArray[E]](coll.asInstanceOf[Array[E]])
	   with IndexedSeqOps[E, IRefArray, IArray[E]]
{
	override def isImmutable :Boolean = true

	override def removed(index :Int) :IArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :IArray[E] = coll.removed(from, until)

	override def empty :IArray[E] = IArray.empty[E]
	protected override def fromSpecific(coll :IterableOnce[E]) :IArray[E] = IArray.from(coll)
	protected override def newSpecificBuilder :Builder[E, IArray[E]] =
		IArray.newBuilder(coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IArray.Wrapped(coll)

	override def iterableFactory :IterableFactory[IRefArray] = IRefArray

	private implicit def classTag :ClassTag[E] = ClassTag(coll.getClass.getComponentType)
	override def className = "IArray"
}






/** A non-sticky adapter of a `RefArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, RefArray, RefArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * is that they are standard `Seq` implementations, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class RefArrayAsSeq[E](override val coll :RefArray[E])
	extends AbstractArrayAsSeq[E, RefArray, RefArray[E]](coll.asInstanceOf[Array[E]])
{
	override def removed(index :Int) :RefArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :RefArray[E] = coll.removed(from, until)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :RefArray[E] = RefArray.from(coll)
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, RefArray[E]] = RefArray.newBuilder
	override def iterableFactory :IterableFactory[RefArray] = RefArray

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll.asInstanceOf[Array[E]])
}






/** A non-sticky adapter of an `IRefArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, IRefArray, IRefArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * is that they are standard `Seq` implementations, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class IRefArrayAsSeq[E](override val coll :IRefArray[E])
	extends AbstractArrayAsSeq[E, IRefArray, IRefArray[E]](coll.asInstanceOf[Array[E]])
	   with IndexedSeqOps[E, IRefArray, IRefArray[E]]
{
	override def removed(index :Int) :IRefArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :IRefArray[E] = coll.removed(from, until)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :IRefArray[E] = IRefArray.from(coll)
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, IRefArray[E]] = IRefArray.newBuilder
	override def iterableFactory :IterableFactory[IRefArray] = IRefArray

	override def isImmutable :Boolean = true
	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped(coll)
}






/** A non-sticky adapter of a `String` to `IterableOnce[E]` and `IndexedSeqOps[E, IndexedSeq, String]`.
  * All operations return a `String`, or a generic `IndexedSeq` if element type changes, not another instance
  * of this class. What makes it different from standard extension methods in [[scala.collection.StringOps StringOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.WrappedString WrappedString]] return the same sequence type when filtering or mapping.
  * It's useful for enabling the use of strings as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `String`s.
  */ //todo: add an implicit conversion somewhere
@SerialVersionUID(Ver)
private final class StringAsSeq(override val coll :String)
	extends IterableOnce[Char] with IndexedSeqOps[Char, IndexedSeq, String]
{
	def length :Int = coll.length

	override def apply(i :Int) :Char = coll.charAt(i)

	override def iterator :Iterator[Char] = new StringIterator(coll, 0, coll.length)

	override def empty :String = ""

	protected override def fromSpecific(coll :IterableOnce[Char]) :String = coll match {
		case empty :Iterable[Char] if empty.isEmpty => ""
		case _ => (new StringBuilder ++= coll).result()
	}
	protected override def newSpecificBuilder :Builder[Char, String] = new StringBuilder
	override def iterableFactory :IterableFactory[IndexedSeq] = IndexedSeq

	override def toIterable :Iterable[Char] = coll

	override def equals(that :Any) :Boolean = that match {
		case it :StringAsSeq => it.coll == coll
		case _ => false
	}
	override def hashCode :Int = coll.hashCode
	override def toString :String = "StringAsSeq(" + coll + ")"
}
