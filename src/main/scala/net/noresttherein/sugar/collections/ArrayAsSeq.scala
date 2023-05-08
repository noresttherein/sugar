package net.noresttherein.sugar.collections

import scala.Array.{emptyBooleanArray, emptyByteArray, emptyCharArray, emptyDoubleArray, emptyFloatArray, emptyIntArray, emptyLongArray, emptyObjectArray, emptyShortArray}
import scala.annotation.nowarn
import scala.collection.{ClassTagIterableFactory, IterableFactory, mutable}
import scala.collection.immutable.{ArraySeq, IndexedSeqOps}
import scala.collection.mutable.{ArrayBuilder, Builder, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.extensions.castTypeParam

//extension methods
import extensions._





/** A non-sticky adapter of an `Array[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, Array, Array[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * is that they are standard `Seq` implementations, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class ArrayAsSeq[E](override val coll :Array[E])
	extends IterableOnce[E] with mutable.IndexedSeqOps[E, Array, Array[E]]
{
	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem
	override def apply(i :Int) :E = coll(i)

	override def length :Int = coll.length

	override def iterator :Iterator[E] = new ArrayIterator[E](coll, 0, coll.length)

	override def empty :Array[E] =
		ArrayAsSeq.empty(coll.getClass.getComponentType.castParam[E])

	protected override def fromSpecific(coll :IterableOnce[E]) :Array[E] =
		ArrayAsSeq.from(coll)(classTag)

	protected override def newSpecificBuilder :Builder[E, Array[E]] =
		ArrayAsSeq.newBuilder[E](coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

	override def iterableFactory :IterableFactory[Array] = ErasedArray

	private def classTag = ClassTag(coll.getClass.getComponentType).asInstanceOf[ClassTag[E]]

	override def equals(that :Any) :Boolean = that match {
		case other :ArrayAsSeq[_] => coll sameElements other.coll
		case other :IArrayAsSeq[_] => other.coll sameElements coll
		case it :IterableOnce[_] =>
			mutable.ArraySeq.make(coll) == it
		case _ => false
	}
	override def hashCode :Int = mutable.ArraySeq.make(coll).hashCode
	override def toString :String = mutable.ArraySeq.make(coll).mkString("Array(", ", ", ")")
}




@SerialVersionUID(Ver)
private object ArrayAsSeq extends ClassTagIterableFactory[Array] {

	def wrap[E](array :Array[E]) :mutable.IndexedSeqOps[E, Array, Array[E]] =
		new ArrayAsSeq(array)

	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
		case elems :Iterable[E] if elems.knownSize == 0 => Array.empty[E]
		case iter  :Iterator[E] if !iter.hasNext => Array.empty[E]
		case elems :ArrayAsSeq[E] if elems.coll.getClass.getComponentType == classTag[E].runtimeClass => elems.coll
		case elems :Iterable[E] => elems.toArray[E]
		case _ => it.iterator.toArray[E]
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

	override def empty[A :ClassTag] :Array[A] = Array.empty[A]

	override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder[A] //new ArrayBuilder[A]

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


	val untagged :IterableFactory[Array] = ErasedArray

	private val InitialBuilderSize = 16

	/** Our own version of a builder for  */
	private class ArrayBuilder[A](elemType :Class[A]) extends ReusableBuilder[A, Array[A]] {
		def this()(implicit tag :ClassTag[A]) = this(tag.runtimeClass.castParam[A])

		private[this] var buffer :Array[A] = _
		private[this] var size :Int = 0

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
			buffer = null
			size = 0
		}

		override def result() =
			if (size == 0)
				ArrayAsSeq.empty(elemType)
			else if (size == buffer.length)
				buffer
			else {
				val res = Array.copyOf(buffer, size)
				buffer = null
				size = 0
				res
			}
	}
}






/** A non-sticky adapter of an `IArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, IArray, IArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.ArrayOps ArrayOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.ArraySeq ArraySeq]] and [[scala.collection.mutable.ArraySeq mutable.ArraySeq]]
  * is that they are standard `Seq` implementations, creating the same `ArraySeq` type when filtering or mapping.
  * It's useful for enabling the use of arrays as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `Array`s.
  */
@SerialVersionUID(Ver)
private final class IArrayAsSeq[E](override val coll :IArray[E])
	extends IterableOnce[E] with IndexedSeqOps[E, IArray, IArray[E]]
{
	override def apply(i :Int) :E = coll(i)

	override def length :Int = coll.length

	override def iterator :Iterator[E] = new ArrayIterator(coll.asInstanceOf[Array[E]], 0, coll.length)

	override def empty :IArray[E] =
		IArray.empty(classTag)

	protected override def fromSpecific(coll :IterableOnce[E]) :IArray[E] =
		IArray.from(coll)(classTag)

	protected override def newSpecificBuilder :Builder[E, IArray[E]] =
		IArray.newBuilder[E](classTag)

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = ArraySeq.unsafeWrapArray(coll.asInstanceOf[Array[E]])

	override def iterableFactory :IterableFactory[IArray] = IArray.untagged

	private def classTag = ClassTag(coll.getClass.getComponentType).asInstanceOf[ClassTag[E]]

	override def equals(that :Any) :Boolean = that match {
		case other :IArrayAsSeq[_] => coll sameElements other.coll
		case other :ArrayAsSeq[_] => coll sameElements other.coll
		case it :IterableOnce[_] =>
			ArraySeq.unsafeWrapArray(coll.asInstanceOf[Array[E]]) == it
		case _ => false
	}
	override def hashCode :Int = ArraySeq.unsafeWrapArray(coll.asInstanceOf[Array[E]]).hashCode
	override def toString :String = ArraySeq.unsafeWrapArray(coll.asInstanceOf[Array[E]]).mkString("Array(", ", ", ")")
}
//
//
//
//
//@SerialVersionUID(Ver)
//object IArrayAsSeq extends ClassTagIterableFactory[IArray] {
//
//	def wrap[E](array :IArray[E]) :IndexedSeqOps[E, IArray, IArray[E]] = new IArrayAsSeq(array)
//
//	override def from[E :ClassTag](it :IterableOnce[E]) :IArray[E] = IArray.from(it)
//
//	override def empty[E :ClassTag] :IArray[E] = IArray.empty[E]
//
//	def empty[E](elementType :Class[E]) :IArray[E] = IArray.empty(elementType)
//
//	override def newBuilder[E :ClassTag] :Builder[E, IArray[E]] = IArray.newBuilder[E]
//
//	def newBuilder[E](elementType :Class[E]) :Builder[E, IArray[E]] = IArray.newBuilder(elementType)
//
//	val untagged :IterableFactory[IArray] = IArray.untagged
//}






/** A non-sticky adapter of a `String` to `IterableOnce[E]` and `IndexedSeqOps[E, IndexedSeq, String]`.
  * All operations return the resulting `Array[E]`, not another instance of this class.
  * What makes it different from standard extension methods in [[scala.collection.StringOps StringOps]]
  * is that the latter is not an `IterableOnce`. On the other hand, explicitly created
  * [[scala.collection.immutable.WrappedString WrappedString]] return the same sequence type when filtering or mapping.
  * It's useful for enabling the use of strings as parameters to any method/class requiring an `IterableOps[E, CC, C]`,
  * so that the result(s) are also `String`s.
  */
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
