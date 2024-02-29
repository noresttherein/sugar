package net.noresttherein.sugar.arrays

import java.util.Arrays

import scala.Array.{emptyBooleanArray, emptyByteArray, emptyCharArray, emptyDoubleArray, emptyFloatArray, emptyIntArray, emptyLongArray, emptyObjectArray, emptyShortArray}
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ReusableBuilder}
import scala.collection.{ClassTagIterableFactory, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.{illegal_!, maxSize_!}
import net.noresttherein.sugar.casting.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, BufferFullException, Builders, ViewBuffer}
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.extensions.IterableOnceExtension
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.reflect.Specialized.NotUnit
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}





/** An `EvidenceIterableFactory[Array]` to which the `Array` object is implicitly converted.
  * Because `classOf[Unit] == java.lang.Void.TYPE`, and a `void[]` does not exist, copying methods
  * consistently handle requests for `Unit` arrays by creating an `Array[BoxedUnit]` and initializing
  * it with the unit value - rather than `null` - as if it was a true primitive type.
  */
@SerialVersionUID(Ver)
object ArrayFactory extends ClassTagIterableFactory[Array] {
//	@inline override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = Array.from(it)

	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
		case elems :Iterable[E] if elems.knownSize == 0 => empty[E]
		case iter  :Iterator[E] if !iter.hasNext        => empty[E]
//		case elems :ArrayAsSeq[E] if elems.coll.getClass.getComponentType == classTag[E].runtimeClass => elems.coll
		case elems :Iterable[E]                         => elems.toArray[E]
		case _                                          => it.iterator.toArray[E]
	}

	@inline override def empty[A :ClassTag] :Array[A] = Array.empty

	def empty[A](elementType :Class[A]) :Array[A] = {
		if (elementType == classOf[AnyRef]) emptyObjectArray
		else if (elementType.isPrimitive)
			if (elementType == classOf[Int]) emptyIntArray
			else if (elementType == classOf[Long]) emptyLongArray
			else if (elementType == classOf[Double]) emptyDoubleArray
			else if (elementType == classOf[Byte]) emptyByteArray
			else if (elementType == classOf[Char]) emptyCharArray
			else if (elementType == classOf[Float]) emptyFloatArray
			else if (elementType == classOf[Short]) emptyShortArray
			else if (elementType == classOf[Boolean]) emptyBooleanArray
			else if (elementType == classOf[Unit]) emptyUnitArray
			else illegal_!("Cannot create an array of " + elementType.name)
		else if (elementType == classOf[BoxedUnit]) emptyUnitArray
		else java.lang.reflect.Array.newInstance(elementType, 0)
	}.asInstanceOf[Array[A]]

	private val emptyUnitArray = Array.emptyUnitArray

	final val MaxSize = Int.MaxValue - 8 //According to java.util.ArrayDeque, and it should know

	@inline def emptyLike[A](template :Array[A]) :Array[A] = empty(template.getClass.getComponentType.castParam[A])

	/** Creates an array of the specified element type and length. Similar to `Array.ofDim[E](n)`,
	  * but allows specifying the element type explicitly, and, additionally, initializes `Array[Unit]`.
	  */
	def ofDim[E](elementType :Class[E], n :Int) :Array[E] =
		if (n == 0)
			empty(elementType)
		else if (elementType == classOf[Unit] || elementType == classOf[BoxedUnit])
			Array.copyOf(emptyUnitArray, n).castFrom[Array[Unit], Array[E]]
		else
			java.lang.reflect.Array.newInstance(elementType, n).castFrom[AnyRef, Array[E]]

	/** Creates an `Array[E]` based on an implicit class tag for `E`, of the same length that the argument
	  * and copies the contents.
	  */
	@inline final def copyAs[E :ClassTag](array :ArrayLike[_]) :Array[E] =
		copyAs(array, classTag[E].runtimeClass.asInstanceOf[Class[E]], array.asInstanceOf[Array[_]].length)

	/** Same as `Array.copyAs`, but always resorts to `System.arraycopy` if both the input and output element types
	  * are reference types. It also initializes returned arrays with the `Unit` value if `E =:= Unit`.
	  */
	@inline def copyAs[E](array :ArrayLike[_], newLength :Int)(implicit classTag :ClassTag[E]) :Array[E] =
		copyAs(array, classTag.runtimeClass.asInstanceOf[Class[E]], newLength)

	/** Creates a new array with all elements of the given array, but of a new element type (component class).
	  * @return [[net.noresttherein.sugar.arrays.ArrayFactory.copyAs copyAs]]`(array, newElementClass, array.length)`.
	  */
	@inline def copyAs[E](array :ArrayLike[_], newElementClass :Class[_]) :Array[E] =
		copyAs(array, newElementClass, array.asInstanceOf[Array[_]].length)

	/** Creates an array of the given length and component class, and copies to it data from `array`.
	  * The specified `elementType` needs not to match the class behind `array`, as long as it is a superclass
	  * of all elements actually stored in the latter. If `elementClass =:= classOf[Unit]`,
	  * then the array is filled with the unit value (rather than `null`, as it would be by the Java's API).
	  * This method offers some minor optimizations which may be visible with small array sizes.
	  */
	def copyAs[E](array :ArrayLike[_], newElementClass :Class[_], newLength :Int) :Array[E] =
		if (newLength == 0)
			empty(newElementClass.castParam[E])
		else if (newElementClass == Void.TYPE || newElementClass == classOf[BoxedUnit]) {
			val res = new Array[Unit](newLength)
			res.fill(())
			res.castFrom[Array[Unit], Array[E]]
		} else if (newElementClass isAssignableFrom array.getClass.getComponentType)
			if (newElementClass.isPrimitive) //array.getClass.getComponentType must be the same primitive type
				Array.copyOf(array.castFrom[ArrayLike[_], Array[E]], newLength)
			else
				Arrays.copyOf(
					array.castFrom[ArrayLike[_], Array[AnyRef]],
					newLength,
					ArrayClass(newElementClass).castParam[Array[AnyRef]]
				).castFrom[Array[AnyRef], Array[E]]
		else {
			val oldLength = array.asInstanceOf[Array[_]].length
			val res = java.lang.reflect.Array.newInstance(newElementClass, newLength).asInstanceOf[Array[E]]
			ArrayLike.copy(array, 0, res, 0, math.min(oldLength, newLength))
			res
		}


	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyAs copyAs]]`(array, elementClass)`,
	  * but infers the element type from the `elementClass` type parameter.
	  */
	@inline def copyOf[E](array :ArrayLike[_], newElementClass :Class[E]) :Array[E] =
		copyAs(array, newElementClass, array.asInstanceOf[Array[_]].length)

	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyAs copyAs]]`(array, elementClass, newLength)`,
	  * but infers the element type from the `elementClass` type parameter.
	  */
	@inline def copyOf[E](array :ArrayLike[_], newElementClass :Class[E], newLength :Int) :Array[E] =
		copyAs(array, newElementClass, newLength)

	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyOf copyAs]]`(array)`,
	  * but the argument array-like is of the requested element type, leading to `E` being inferred.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :Array[E] =
		copyAs[E](array, classTag[E].runtimeClass, array.asInstanceOf[Array[_]].length)

	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyOf copyAs]]`(array, newLength)`,
	  * but the argument array-like is of the requested element type, leading to `E` being inferred.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :Array[E] =
		copyAs[E](array, classTag[E].runtimeClass, newLength)


	@inline def copyOf[E](array :Array[E]) :Array[E] = {
		val length = array.length
		if (length == 0) array
		else Array.copyOf(array, length)
	}

	@inline def copyOf[E](array :Array[E], newLength :Int) :Array[E] =
		if (newLength == 0) empty(array.getClass.getComponentType.castParam[E])
		else Array.copyOf(array, newLength)

/*
	def copyOf[E](array :Array[E], start :Int, newLength :Int) :Array[E] =
		if (start == 0)
			copyOf(array, newLength)
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				"ArrayAsSeq.copyOf(" + array.className + "<" + array.length + ">, " + start + ", " + newLength + ")"
			)
		else if (newLength == 0)
			empty(array.getClass.getComponentType.castParam[E])
		else {
			val res = Array.of(array.getClass.getComponentType.castParam[E], newLength)
			if (start < newLength)
				ArrayLike.copy(array, 0, res, start, math.min(newLength - start, array.length))
			res
		}

	def copyOfRange[E](array :Array[E], from :Int, until :Int) :Array[E] =
		if (until <= from)
			empty(array.getClass.getComponentType.castParam[E])
		else
			(((array :Array[_]) : @unchecked) match {
				case a :Array[AnyRef]  => Arrays.copyOfRange(a, from, until)
				case a :Array[Int]     => Arrays.copyOfRange(a, from, until)
				case a :Array[Long]    => Arrays.copyOfRange(a, from, until)
				case a :Array[Double]  => Arrays.copyOfRange(a, from, until)
				case a :Array[Byte]    => Arrays.copyOfRange(a, from, until)
				case a :Array[Char]    => Arrays.copyOfRange(a, from, until)
				case a :Array[Float]   => Arrays.copyOfRange(a, from, until)
				case a :Array[Short]   => Arrays.copyOfRange(a, from, until)
				case a :Array[Boolean] => Arrays.copyOfRange(a, from, until)
			}).asInstanceOf[Array[E]]
*/

	def byteBuilder :Builders.fromBytes[Array[Byte]] =
		new ArrayBuilder[Byte](classOf[Byte]) with Builders.fromBytes[Array[Byte]] 
	
	def charBuilder :Builders.fromChars[Array[Char]] = 
		new ArrayBuilder[Char](classOf[Char]) with Builders.fromChars[Array[Char]] 
	
	def shortBuilder :Builders.fromShorts[Array[Short]] = 
		new ArrayBuilder[Short](classOf[Short]) with Builders.fromShorts[Array[Short]] 
	
	def intBuilder :Builders.fromInts[Array[Int]] = 
		new ArrayBuilder[Int](classOf[Int]) with Builders.fromInts[Array[Int]] 
	
	def longBuilder :Builders.fromLongs[Array[Long]] = 
		new ArrayBuilder[Long](classOf[Long]) with Builders.fromLongs[Array[Long]] 
	
	def floatBuilder :Builders.fromFloats[Array[Float]] = 
		new ArrayBuilder[Float](classOf[Float]) with Builders.fromFloats[Array[Float]] 
	
	def doubleBuilder :Builders.fromDoubles[Array[Double]] = 
		new ArrayBuilder[Double](classOf[Double]) with Builders.fromDoubles[Array[Double]] 
	
	def booleanBuilder :Builders.fromBooleans[Array[Boolean]] = 
		new ArrayBuilder[Boolean](classOf[Boolean]) with Builders.fromBooleans[Array[Boolean]] 
	
	def refBuilder[E <: AnyRef :ClassTag] :Builder[E, Array[E]] = new ArrayBuilder[E]

	def refBuilder[E <: AnyRef](elementType :Class[E]) :Builder[E, Array[E]] =
		new ArrayBuilder(elementType)

	override def newBuilder[E :ClassTag] :Builder[E, Array[E]] = new ArrayBuilder[E]
	
	def newBuilder[E](elementType :Class[E]) :Builder[E, Array[E]] =
		if (elementType == classOf[AnyRef])
			new ArrayBuilder.ofRef()(ClassTag.AnyRef).asInstanceOf[Builder[E, Array[E]]]
		else if (classOf[AnyRef] isAssignableFrom elementType)
			new this.ArrayBuilder(elementType)
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
		}.asInstanceOf[Builder[E, Array[E]]]

	@inline def newBuilderLike[A](template :Array[A]) :Builder[A, Array[A]] =
		newBuilder(template.getClass.getComponentType.castParam[A])

	private val InitialBuilderSize = 16

	//Consider: making it private, or at least an interface.
	//Todo: addAll(array :ArrayLike, offset :Int, length :Int) and addAll(array :TypedArray[E], offset length)
	/** Our own version of a builder. Calls `releaseFence` before returning the array for safe use with `IArrayLike`.  */
	private class ArrayBuilder[@specialized(NotUnit) E](private[this] var init :Array[E], elemType :Class[E])
		extends ReusableBuilder[E, Array[E]]
	{
		def this(elemType :Class[E]) = this(null, elemType)
		def this()(implicit tag :ClassTag[E]) = this(tag.runtimeClass.castParam[E])
		def this(buffer :Array[E]) = this(buffer, buffer.getClass.getComponentType.castParam[E])

		private[this] var buffer = init
		private[this] var size :Int = if (buffer == null) 0 else buffer.length

		private def maxArraySizeExceeded(extra :Int) =
			if (size == MaxSize) maxSize_!("Maximum array size reached")
			else maxSize_!("Cannot add " + extra + " elements to " + size.toString + ": maximum array size exceeded.")

		override def sizeHint(size :Int) :Unit =
			if (size > this.size & size <= MaxSize)
				if (buffer == null)
					buffer = Array.of(elemType, size)
				else
					buffer = Array.copyOf(buffer, size)

		override def addAll(xs :IterableOnce[E]) :this.type =
			xs.knownSize match {
				case  0 => this
				case -1 => super.addAll(xs)
				case extra if buffer == null =>
					buffer = Array.of(elemType, extra)
					xs.toBasicOps.copyToArray(buffer)
					this
				case extra if extra > MaxSize - size =>
					maxArraySizeExceeded(extra)
				case extra if size + extra <= buffer.length =>
					xs.toBasicOps.copyToArray(buffer, size)
					size += extra
					this
				case extra =>
					val newCapacity = math.max(size + extra, math.min(buffer.length, MaxSize >> 1) << 1)
					buffer = Array.copyOf(buffer, newCapacity)
					xs.toBasicOps.copyToArray(buffer, size)
					this
			}

		override def addOne(elem :E) = {
			if (buffer == null)
				buffer = Array.of(elemType, InitialBuilderSize)
			else if (size == buffer.length) {
				if (size == MaxSize)
					throw new BufferFullException("Maximum array size reached.")
				buffer = Array.copyOf(buffer, math.min(MaxSize >> 1, buffer.length) << 1)
			}
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
			if (size == 0) {
				val res = ArrayFactory.empty(elemType)
				releaseFence()
				res
			} else if (size == buffer.length && (buffer ne init)) {
				releaseFence()
				buffer
			} else {
				val res = Array.copyOf(buffer, size)
				init = null
				buffer = null
				size = 0
				releaseFence()
				res
			}
	}


	//todo: move Array factory methods to ArrayLikeOps (if private) or ArrayLike (if public).
	/** Same as `Array.ofDim(n)`, but also initializes `Array[Unit]` with the `Unit` value. */
	@inline def ofDim[E :ClassTag](n :Int) :Array[E] =
		if (n == 0)
			empty[E]
		else {
			val E = classTag[E].runtimeClass
			if (E == classOf[Unit] || E == classOf[BoxedUnit])
				Array.copyOf(emptyUnitArray, n).castFrom[Array[Unit], Array[E]]
			else
				new Array[E](n)
		}


	/** Wraps and unwraps mutable collections backed by arrays, such as `mutable.ArraySeq`.
	  * Modification of the array will be visible in the wrapped/unwrapped collection,
	  * and vice versa. The pattern will match only if the component type of the underling array equals
	  * the runtime class of the collection's type parameter, as specified by a `ClassTag`.
	  */
	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[A](array :Array[A]) :mutable.IndexedSeq[A] = mutable.ArraySeq.make(array)

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[Array[E]] = {
			val tag = classTag[E]
			val array = elems match {
				case seq :mutable.ArraySeq[_] =>
					Yes(seq.array)
				case seq :ArrayBuffer[_] if seq.length == CheatedAccess.array(seq) =>
					Yes(CheatedAccess.array(seq))
				case slice :ArrayIterableOnce[_] if slice.isMutable && slice.knownSize == slice.unsafeArray.length =>
					Yes(slice.unsafeArray)
				case _ => No
			}
			if (array.isDefined && array.get.getClass.getComponentType == tag.runtimeClass)
				array.castParam[Array[E]]
			else
				No
		}

		/** Factory of views on slices of arrays as indexed sequences, and unwraps known mutable collections
		  * backed by array slices. Modification of the array will be visible in the wrapped/unwrapped collection,
		  * and vice versa. The pattern will match only if the component type of the underling array equals
		  * the runtime class of the collection's type parameter, as specified by a `ClassTag`.
		  */
		@SerialVersionUID(Ver)
		object Slice {
			def apply[E](array :Array[E], from :Int, until :Int) :mutable.IndexedSeq[E] =
				ViewBuffer.full(array, from, until)

			def unapply[E :ClassTag](elems :IterableOnce[E]) :Maybe[(Array[E], Int, Int)] = {
				val tag = classTag[E]
				val expectedClass = tag.runtimeClass
				elems match {
					case seq :mutable.ArraySeq[_] if expectedClass == seq.array.getClass.getComponentType =>
						Yes((seq.array.castParam[E], 0, seq.array.length))
					case seq :ArrayBuffer[_] if expectedClass == CheatedAccess.array(seq).getClass.getComponentType =>
						Yes((CheatedAccess.array(seq).castParam[E], 0, seq.length))
					case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isMutable =>
						val array = slice.unsafeArray
						if (expectedClass == array.getClass.getComponentType)
							Yes((array.castParam[E], slice.startIndex, slice.startIndex + slice.knownSize))
						else
							No
					case _ =>
						No
				}
			}
		}
	}

}
