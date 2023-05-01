package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{mutable, ClassTagIterableFactory, IterableFactory}
import scala.collection.immutable.IndexedSeqOps
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.ClassTag




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
	def update(idx :Int, elem :E) :Unit = coll(idx) = elem
	def apply(i :Int) :E = coll(i)

	def length :Int = coll.length

	override def iterator :Iterator[E] = new ArrayIterator[E](coll, 0, coll.length)

	override def empty :Array[E @uncheckedVariance] =
		ArrayAsSeq.empty(classTag)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :Array[E @uncheckedVariance] =
		ArrayAsSeq.from(coll)(classTag)

	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, Array[E @uncheckedVariance]] =
		ArrayAsSeq.newBuilder[E](classTag)

	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

	override def iterableFactory :IterableFactory[Array] = ArrayAsSeq.untagged

	private def classTag = ClassTag(coll.getClass.getComponentType).asInstanceOf[ClassTag[E]]

	override def equals(that :Any) :Boolean = that match {
		case it :IterableOnce[_] =>
			mutable.ArraySeq.make(coll) == it
		case _ => false
	}
	override def hashCode :Int = mutable.ArraySeq.make(coll).hashCode
	override def toString :String = mutable.ArraySeq.make(coll).mkString("Array(", ", ", ")")
}




@SerialVersionUID(Ver)
object ArrayAsSeq extends ClassTagIterableFactory[Array] {

	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
		case elems :Iterable[E] if elems.isEmpty => Array.empty[E]
		case iter  :Iterator[E] if !iter.hasNext => Array.empty[E]
		case elems :ArrayAsSeq[E] => elems.coll
		case elems :Iterable[E] => it.knownSize match {
			case unknown if unknown < 0 => (newBuilder[E] ++= it).result()
			case size =>
				val res = Array.ofDim[E](size)
				elems.copyToArray(res)
				res
		}
		case _ =>
			val iter = it.iterator
			it.knownSize match {
				case unknown if unknown < 0 => (newBuilder[E] ++= it).result()
				case size =>
					val res = Array.ofDim[E](size)
					iter.copyToArray(res)
					res
			}
	}

	override def empty[A :ClassTag] :Array[A] = Array.empty[A]

	override def newBuilder[A :ClassTag] :Builder[A, Array[A]] = new ArrayBuilder[A]

	object untagged extends IterableFactory[Array] {
		private[this] val emptyAnyRef = new Array[AnyRef](0)
		override def empty[A] :Array[A] = emptyAnyRef.asInstanceOf[Array[A]]
		override def from[A](source :IterableOnce[A]) :Array[A] =
			ArrayAsSeq.from(source)(ClassTag.Any.asInstanceOf[ClassTag[A]])

		override def newBuilder[A] :Builder[A, Array[A]] =
			new ArrayBuilder[A]()(ClassTag.Any.asInstanceOf[ClassTag[A]])
	}

	private val InitialBuilderSize = 16


	private class ArrayBuilder[A :ClassTag] extends ReusableBuilder[A, Array[A]] {
		private[this] var buffer :Array[A] = _
		private[this] var size :Int = 0

		override def sizeHint(size :Int) :Unit =
			if (size > 0) {
				if (buffer == null)
					buffer = Array.ofDim(size)
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
						buffer = Array.ofDim(extra)
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
							buffer = Array.ofDim(extra)
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
				buffer = Array.ofDim[A](InitialBuilderSize)
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
				Array.empty[A]
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
