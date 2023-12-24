package net.noresttherein.sugar.arrays

import java.util.Arrays

import scala.Array.{emptyBooleanArray, emptyByteArray, emptyCharArray, emptyDoubleArray, emptyFloatArray, emptyIntArray, emptyLongArray, emptyObjectArray, emptyShortArray}
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, Builder, ReusableBuilder}
import scala.collection.{ClassTagIterableFactory, mutable}
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.arrays.extensions.{ArrayExtension, ArrayLikeExtension, ArrayCompanionExtension, MutableArrayExtension}
import net.noresttherein.sugar.collections.{ArrayIterableOnce, ViewBuffer}
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.casting.extensions.{castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}





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
			else throw new IllegalArgumentException("Cannot create an array of " + elementType.name)
		else if (elementType == classOf[BoxedUnit]) emptyUnitArray
		else java.lang.reflect.Array.newInstance(elementType, 0)
	}.asInstanceOf[Array[A]]

	private val emptyUnitArray = Array.emptyUnitArray

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

	/** Creates an array of the given length and component class, and copies to it data from `array`.
	  * The specified `elementType` needs not to match the class behind `array`, as long as it is a superclass
	  * of all elements actually stored in the latter. If `elementClass =:= classOf[Unit]`,
	  * then the array is filled with the unit value (rather than `null`, as it would be by the Java's API).
	  * This method offers some minor optimizations which may be visible with small array sizes.
	  */
	def copyAs[E](array :ArrayLike[_], elementClass :Class[E], newLength :Int) :Array[E] =
		if (newLength == 0)
			empty[E](elementClass)
		else if (elementClass == Void.TYPE || elementClass == classOf[BoxedUnit]) {
			val res = new Array[Unit](newLength)
			res.fill(())
			res.castFrom[Array[Unit], Array[E]]
		} else if (elementClass isAssignableFrom array.getClass.getComponentType)
			if (elementClass.isPrimitive)
				Array.copyOf(array.castFrom[ArrayLike[_], Array[E]], newLength)
			else
				Arrays.copyOf(
					array.castFrom[ArrayLike[_], Array[AnyRef]],
					newLength,
					ArrayClass(elementClass).castParam[Array[AnyRef]]
				).castFrom[Array[AnyRef], Array[E]]
		else {
			val oldLength = array.asInstanceOf[Array[_]].length
			val res = java.lang.reflect.Array.newInstance(elementClass, newLength).asInstanceOf[Array[E]]
			ArrayLike.copy(array, 0, res, 0, math.min(oldLength, newLength))
			res
		}

	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyAs copyAs]]`(array)`,
	  * but the argument array-like is of the requested element type, leading to `E` being inferred.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :Array[E] =
		copyAs(array, classTag[E].runtimeClass.asInstanceOf[Class[E]], array.asInstanceOf[Array[_]].length)

	/** Same as [[net.noresttherein.sugar.arrays.ArrayFactory.copyAs copyAs]]`(array, newLength)`,
	  * but the argument array-like is of the requested element type, leading to `E` being inferred.
	  */
	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :Array[E] = copyAs(array, newLength)


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

	@inline override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder

	def newBuilder[A](elementType :Class[A]) :Builder[A, Array[A]] =
		if (elementType == classOf[AnyRef])
			new ArrayBuilder.ofRef()(ClassTag.AnyRef).asInstanceOf[Builder[A, Array[A]]]
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
		}.asInstanceOf[Builder[A, Array[A]]]

	@inline def newBuilderLike[A](template :Array[A]) :Builder[A, Array[A]] =
		newBuilder(template.getClass.getComponentType.castParam[A])

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
					buffer = Array.of(elemType, size)
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
						buffer = Array.of(elemType, extra)
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
							buffer = Array.of(elemType, extra)
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
				buffer = Array.of(elemType, InitialBuilderSize)
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
				ArrayFactory.empty(elemType)
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

		def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[Array[E]] = {
			val tag = classTag[E]
			val array = elems match {
				case seq :mutable.ArraySeq[_] =>
					Got(seq.array)
				case seq :ArrayBuffer[_] if seq.length == CheatedAccess.array(seq) =>
					Got(CheatedAccess.array(seq))
				case slice :ArrayIterableOnce[_] if slice.isMutable && slice.knownSize == slice.unsafeArray.length =>
					Got(slice.unsafeArray)
				case _ => Lack
			}
			if (array.isDefined && array.get.getClass.getComponentType == tag.runtimeClass)
				array.castParam[Array[E]]
			else
				Lack
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

			def unapply[E :ClassTag](elems :IterableOnce[E]) :Opt[(Array[E], Int, Int)] = {
				val tag = classTag[E]
				val expectedClass = tag.runtimeClass
				elems match {
					case seq :mutable.ArraySeq[_] if expectedClass == seq.array.getClass.getComponentType =>
						Got((seq.array.castParam[E], 0, seq.array.length))
					case seq :ArrayBuffer[_] if expectedClass == CheatedAccess.array(seq).getClass.getComponentType =>
						Got((CheatedAccess.array(seq).castParam[E], 0, seq.length))
					case slice :ArrayIterableOnce[E] if elems.knownSize >= 0 && slice.isMutable =>
						val array = slice.unsafeArray
						if (expectedClass == array.getClass.getComponentType)
							Got((array.castParam[E], slice.startIndex, slice.startIndex + slice.knownSize))
						else
							Lack
					case _ =>
						Lack
				}
			}
		}
	}

}
