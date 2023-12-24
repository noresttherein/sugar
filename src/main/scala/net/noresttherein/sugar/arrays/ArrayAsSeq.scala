package net.noresttherein.sugar.arrays

import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{ClassTagIterableFactory, IterableFactory, mutable}
import scala.collection.immutable.IndexedSeqOps
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.{ArrayIterableOnce, IArrayLikeSlice, SugaredIterableOps}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.reflect.ArrayClass
import net.noresttherein.sugar.typist.casting.extensions.castTypeParamMethods

//extension methods
import extensions._




//todo: make these specialized, and create them only from methods whose signature will be unchanged in Scala 3
private trait ArrayLikeAsSeqOps[E, +CC[_], +C] //why can't it extend ArraySliceOps
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
  *///consider: renaming all to <ArrayType>SeqOps
@SerialVersionUID(Ver)
private class ArrayLikeAsSeq[E](a :ArrayLike[E])
	extends IterableOnce[E] with ArrayIterableOnce[E] with ArrayLikeAsSeqOps[E, ArrayLike, ArrayLike[E]]
	   with Serializable
{
	private[this] val array :Array[E] = a.asInstanceOf[Array[E]]
	override def coll :ArrayLike[E] = array
	override def unsafeArray :Array[_] = array
	override def startIndex :Int = 0
	private[sugar] override def isImmutable :Boolean = false
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
		copyRangeToArray(xs, start, 0, len)

	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
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
		case it :IterableOnce[_]      => mutable.ArraySeq.make(array) == it
		case _                        => false
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
	private[sugar] override def isImmutable :Boolean = true
	protected override def fromSpecific(coll :IterableOnce[E]) :IArrayLike[E] = IArray.from(coll)
	protected override  def newSpecificBuilder :Builder[E, IArrayLike[E]] =
		IArray.newBuilder(coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable      :Iterable[E] = toIndexedSeq
	override def iterableFactory :IterableFactory[IRefArray] = IRefArray
	override def toIndexedSeq    :IndexedSeq[E] = IArrayLikeSlice.wrap(coll)
//	override def toIterable = IArrayLike.Wrapped(coll)
}






/** A non-sticky adapter of an `Array[E]` to `IterableOnce[E]` and `mutable.IndexedSeqOps[E, Array, Array[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class, with the exception
  * of those which potentially change the element type, which return an erased
  * [[net.noresttherein.sugar.arrays.RefArray RefArray]].
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
	private[sugar] override def isMutable = true

	override def update(idx :Int, elem :E) :Unit = coll(idx) = elem

	override def removed(index :Int) :Array[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :Array[E] = coll.removed(from, until)

	override def empty :Array[E] =
		ArrayFactory.empty(coll.getClass.getComponentType.castParam[E])

	protected override def fromSpecific(coll :IterableOnce[E]) :Array[E] = ArrayAsSeq.from(coll)
	protected override def newSpecificBuilder :Builder[E, Array[E]] =
		ArrayFactory.newBuilder[E](coll.getClass.getComponentType.castParam[E])

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

	override def iterableFactory :IterableFactory[RefArray] = RefArray

//	private implicit def classTag :ClassTag[E] = ClassTag(coll.getClass.getComponentType)

	override def className = "Array"
}




@SerialVersionUID(Ver)
private object ArrayAsSeq extends ClassTagIterableFactory[Array] {

	@inline override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = ArrayFactory.from(it)
//	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
//		case elems :Iterable[E] if elems.knownSize == 0 => empty[E]
//		case iter  :Iterator[E] if !iter.hasNext        => empty[E]
////		case elems :ArrayAsSeq[E] if elems.coll.getClass.getComponentType == classTag[E].runtimeClass => elems.coll
//		case elems :Iterable[E]                         => elems.toArray[E]
//		case _                                          => it.iterator.toArray[E]
//	}
//	def fromAs[E](it :IterableOnce[E], elementType :Class[E]) :Array[E] = it match {
//		case elems :Iterable[E] if elems.knownSize == 0 => empty(elementType)
//		case iter  :Iterator[E] if !iter.hasNext        => empty(elementType)
////		case elems :ArrayAsSeq[E] if elems.coll.getClass.getComponentType == classTag[E].runtimeClass => elems.coll
//		case elems :Iterable[E] => elems.toArray[E]
//		case _ => it.iterator.toArray[E]
//	}

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

	override def empty[A :ClassTag] :Array[A] = ArrayFactory.empty

	override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = Array.newBuilder[A] //new ArrayBuilder[A]

//	def newBuilder[A](init :Array[A]) :Builder[A, Array[A]] =
//		new ArrayBuilder(init)

}






/** A non-sticky adapter of an `IArray[E]` to `IterableOnce[E]` and `IndexedSeqOps[E, IArray, IArray[E]]`.
  * All operations return the resulting `Array[E]`, not another instance of this class, with the exception
  * of those which potentially change the element type, which return an erased
  * [[net.noresttherein.sugar.arrays.IRefArray IRefArray]].
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
	private[sugar] override def isMutable = true

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
	private[sugar] override def isImmutable :Boolean = true

	override def removed(index :Int) :IRefArray[E] = coll.removed(index)
	override def removed(from :Int, until :Int) :IRefArray[E] = coll.removed(from, until)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :IRefArray[E] = IRefArray.from(coll)
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, IRefArray[E]] = IRefArray.newBuilder
	override def iterableFactory :IterableFactory[IRefArray] = IRefArray

	@nowarn("cat=deprecation")
	override def toIterable :Iterable[E] = toIndexedSeq
	override def toIndexedSeq :IndexedSeq[E] = IRefArray.Wrapped(coll)
}

