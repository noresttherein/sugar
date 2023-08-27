package net.noresttherein.sugar.collections

import java.util.Arrays

import scala.Array.{emptyBooleanArray, emptyByteArray, emptyCharArray, emptyDoubleArray, emptyFloatArray, emptyIntArray, emptyLongArray, emptyObjectArray, emptyShortArray}
import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{ClassTagIterableFactory, IterableFactory, mutable}
import scala.collection.immutable.IndexedSeqOps
import scala.collection.mutable.{ArrayBuilder, Builder, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.extensions.{ClassExtension, castTypeParamMethods, castingMethods}
import net.noresttherein.sugar.reflect.ArrayClass

//extension methods
import extensions._




//todo: make these specialized, and create them only from methods whose signature will be unchanged in Scala 3
private trait ArrayLikeAsSeqOps[E, +CC[_], +C]
	extends collection.IndexedSeqOps[E, CC, C] with SugaredIterableOps[E, CC, C]


/** A non-sticky adapter of an `ArrayLike[E]` to `IterableOnce[E]` and `collection.IndexedSeqOps[E, Array, Array[E]]`.
  * Non-sticky means here that all operations return some specific `ArrayLike` subtype `CC[A]` or `C`,
  * rather than another collection.
  *
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, methods of
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * create another `ArraySeq` when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private class ArrayLikeAsSeq[E](a :ArrayLike[E])
	extends IterableOnce[E] with AbstractArraySlice[E] with ArrayLikeAsSeqOps[E, ArrayLike, ArrayLike[E]]
	   with Serializable
{
	private[this] val array :Array[E] = a.asInstanceOf[Array[E]]
	override def coll :ArrayLike[E] = array
	override def unsafeArray :Array[_] = array
	override def startIndex :Int = 0
	private[collections] override def isImmutable :Boolean = false
	override def isTraversableAgain = true

	override def length :Int = array.length
	override def apply(i :Int) :E = array(i)
	override def iterator :Iterator[E] = ArrayIterator(array)
//	override def empty = iterableFactory.empty

	@nowarn("cat=deprecation")
	override def toIterable   :Iterable[E]   = ArrayLike.Wrapped(coll)
	override def toSeq        :Seq[E]        = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped(array.toIRefArray)

	protected override def fromSpecific(coll :IterableOnce[E]) :ArrayLike[E] = IArray.from(coll)
	protected override def newSpecificBuilder :Builder[E, ArrayLike[E]] =
		IArray.newBuilder(coll.getClass.getComponentType.castParam[E])

	override def iterableFactory :IterableFactory[ArrayLike] = RefArray


	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		copyRangeToArray(xs, 0, start, len)

	override def copyRangeToArray[A >: E](xs :Array[A], from :Int, start :Int, len :Int) :Int =
		if (len <= 0 || from >= array.length || start >= xs.length)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(
				errorString(array) + ".copyRangeToArray(" + errorString(xs) + ", " + start + ", " + from + ", " + len + ")"
			)
		else {
			val from0 = math.max(from, 0)
			val copied = math.min(len, math.min(xs.length - start, array.length - from0))
			ArrayLike.copy(array, from, xs, start, copied)
			copied
		}

	override def equals(that :Any) :Boolean = that match {
		case other :ArrayLikeAsSeq[_] => coll sameElements other.coll
		case it :IterableOnce[_]   => mutable.ArraySeq.make(array) == it
		case _                     => false
	}

	override def hashCode :Int = mutable.ArraySeq.make(array).hashCode

	protected def className :String = iterableFactory.toString
	override def toString :String = ArrayLike.Wrapped(array).mkString(className + "(", ", ", ")")

	protected implicit def classTag :ClassTag[E] = ClassTag[E](array.getClass.getComponentType)
}




@SerialVersionUID(Ver)
private sealed class IArrayLikeAsSeq[E](override val coll :IArrayLike[E])
	extends ArrayLikeAsSeq[E](coll) with ArrayLikeAsSeqOps[E, IArrayLike, IArrayLike[E]]
	   with IndexedSeqOps[E, IArrayLike, IArrayLike[E]]
{
	private[collections] override def isImmutable :Boolean = true
	protected override def fromSpecific(coll :IterableOnce[E]) :IArrayLike[E] = IArray.from(coll)
	protected override  def newSpecificBuilder :Builder[E, IArrayLike[E]] =
		IArray.newBuilder(coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable      :Iterable[E] = toIndexedSeq
	override def iterableFactory :IterableFactory[IRefArray] = IRefArray
	override def toIndexedSeq    :IndexedSeq[E] = IArrayLikeSlice.of(coll)
//	override def toIterable = IArrayLike.Wrapped(coll)
}






/** A non-sticky adapter of an `Array[E]` to `IterableOnce[E]` and `mutable.IndexedSeqOps[E, Array, Array[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class, with the exception
  * of those which potentially change the element type, which return an erased
  * [[net.noresttherein.sugar.collections.RefArray RefArray]].
  *
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, methods of
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * create another `ArraySeq` when filtering or mapping. It's useful for enabling the use of arrays as parameters
  * to any method/class requiring an `IterableOps[E, CC, C]`, so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class ArrayAsSeq[E](override val coll :Array[E])
	extends ArrayLikeAsSeq[E](coll) with ArrayLikeAsSeqOps[E, RefArray, Array[E]]
	   with mutable.IndexedSeqOps[E, RefArray, Array[E]]
{
	private[collections] override def isMutable = true

	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem

	override def removed(index :Int) :Array[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :Array[E] = coll.removed(from, until)

	override def empty :Array[E] =
		ArrayAsSeq.empty(coll.getClass.getComponentType.castParam[E])

	protected override def fromSpecific(coll :IterableOnce[E]) :Array[E] = ArrayAsSeq.from(coll)
	protected override def newSpecificBuilder :Builder[E, Array[E]] =
		ArrayAsSeq.newBuilder[E](coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

	override def iterableFactory :IterableFactory[RefArray] = RefArray

//	private implicit def classTag :ClassTag[E] = ClassTag(coll.getClass.getComponentType)

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
/*
	def copyAs[E :ClassTag](array :ArrayLike[E], start :Int, newLength :Int) :Array[E] =
		if (start == 0)
			copyAs[E](array, newLength)
		else if (start < 0)
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
*/
	private val emptyUnitArray = new Array[Unit](0)

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

	/** Same as `Array.copyAs`, but more always resorts to `System.arraycopy` if both the input and output element types
	  * are reference types. It also initializes returned arrays with the `Unit` value if `E =:= Unit`.
	  */
	def copyAs[E](array :ArrayLike[_], newLength :Int)(implicit classTag :ClassTag[E]) :Array[E] =
		if (newLength == 0)
			empty[E]
		else {
			val elemClass = classTag.runtimeClass
			if (elemClass isAssignableFrom array.getClass.getComponentType)
				if (elemClass.isPrimitive)
					Array.copyOf(array.castFrom[ArrayLike[_], Array[E]], newLength)
				else if (elemClass == Void.TYPE || elemClass == classOf[BoxedUnit]) {
					val res = new Array[Unit](newLength)
					res.fill(())
					res.castFrom[Array[Unit], Array[E]]
				} else
					Arrays.copyOf(
						array.castFrom[ArrayLike[_], Array[AnyRef]],
						newLength,
						ArrayClass(elemClass).castParam[Array[AnyRef]]
					).castFrom[Array[AnyRef], Array[E]]
			else {
				val oldLength = array.length
				val res = new Array[E](newLength)
				ArrayLike.copy(array, 0, res, 0, math.min(oldLength, newLength))
				(res :Array[_]) match {
					case units :Array[Unit] if oldLength < newLength => units.fill(oldLength, newLength)(())
					case _ =>
				}
				res
			}
		}

	@inline def copyOf[E :ClassTag](array :ArrayLike[E], newLength :Int) :Array[E] = copyAs(array, newLength)

	@inline def copyOf[E :ClassTag](array :ArrayLike[E]) :Array[E] =
		copyAs(array, array.asInstanceOf[Array[_]].length)


	@inline def copyOf[E](array :Array[E]) :Array[E] =
		if (array.length == 0) array
		else Array.copyOf(array, array.length)

	@inline def copyOf[E](array :Array[E], newLength :Int) :Array[E] =
		if (newLength == 0) empty(array.getClass.getComponentType.castParam[E])
		else Array.copyOf(array, array.length)

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
  * On the other hand, methods of [[scala.collection.immutable.ArraySeq ArraySeq]]
  * create another `ArraySeq` when filtering or mapping. It's useful for enabling the use of arrays as parameters
  * to any method/class requiring an `IterableOps[E, CC, C]`, so that the result(s) are also `IArray`s.
  */
@SerialVersionUID(Ver)
private final class IArrayAsSeq[E](override val coll :IArray[E])
	extends IArrayLikeAsSeq[E](coll) with ArrayLikeAsSeqOps[E, IRefArray, IArray[E]]
	   with IndexedSeqOps[E, IRefArray, IArray[E]]
{
	override def removed(index :Int) :IArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :IArray[E] = coll.removed(from, until)

	override def empty :IArray[E] = IArray.empty(coll.getClass.getComponentType.castParam[E])
	protected override def fromSpecific(coll :IterableOnce[E]) :IArray[E] = IArray.from(coll)
	protected override def newSpecificBuilder :Builder[E, IArray[E]] =
		IArray.newBuilder(coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IArray.Wrapped(coll)

//	private implicit def classTag :ClassTag[E] = ClassTag(coll.getClass.getComponentType)

	override def className = "IArray"
}






/** A non-sticky adapter of a `RefArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, RefArray, RefArray[E]]`.
  * All operations return the resulting `RefArray[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, methods of
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * create another, independent `Seq` when filtering or mapping. It's useful for enabling the use of arrays
  * as parameters to any method/class requiring an `IterableOps[E, CC, C]`, so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class RefArrayAsSeq[E](override val coll :RefArray[E])
	extends ArrayLikeAsSeq[E](coll) with ArrayLikeAsSeqOps[E, RefArray, RefArray[E]]
	   with mutable.IndexedSeqOps[E, RefArray, RefArray[E]]
{
	private[collections] override def isMutable = true

	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem

	override def removed(index :Int) :RefArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :RefArray[E] = coll.removed(from, until)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :RefArray[E] = RefArray.from(coll)
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, RefArray[E]] = RefArray.newBuilder
	override def iterableFactory :IterableFactory[RefArray] = RefArray

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = RefArray.Wrapped(coll)
}






/** A non-sticky adapter of an `IRefArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, IRefArray, IRefArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, methods of
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * create another `Seq` when filtering or mapping. It's useful for enabling the use of arrays as parameters
  * to any method/class requiring an `IterableOps[E, CC, C]`, so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class IRefArrayAsSeq[E](override val coll :IRefArray[E])
	extends IArrayLikeAsSeq[E](coll) with ArrayLikeAsSeqOps[E, IRefArray, IRefArray[E]]
	   with IndexedSeqOps[E, IRefArray, IRefArray[E]]
{
	private[collections] override def isImmutable :Boolean = true

	override def removed(index :Int) :IRefArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :IRefArray[E] = coll.removed(from, until)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :IRefArray[E] = IRefArray.from(coll)
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, IRefArray[E]] = IRefArray.newBuilder
	override def iterableFactory :IterableFactory[IRefArray] = IRefArray

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
	extends IterableOnce[Char] with IndexedSeqOps[Char, IndexedSeq, String] with Serializable
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
	override def toString :String = coll //"StringAsSeq(" + coll + ")"
}
