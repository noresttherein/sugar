package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{mutable, ClassTagIterableFactory, IterableFactory, IterableOps}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.ClassTag




private class ArrayLikeSeq[E](protected override val coll :Array[E]) extends IterableOps[E, Array, Array[E]] {
	def update(idx :Int, elem :E) :Unit = coll(idx) = elem
	def apply(i :Int) :E = coll(i)

	def length :Int = coll.length

	override def iterator :Iterator[E] = new ArrayIterator[E](coll, 0, coll.length)

	override def empty :Array[E @uncheckedVariance] =
		ArrayLikeSeq.empty(classTag)

	protected override def fromSpecific(coll :IterableOnce[E @uncheckedVariance]) :Array[E @uncheckedVariance] =
		ArrayLikeSeq.from(coll)(classTag)

	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, Array[E @uncheckedVariance]] =
		ArrayLikeSeq.newBuilder[E](classTag)

	override def toIterable :Iterable[E] = mutable.ArraySeq.make(coll)

	override def iterableFactory :IterableFactory[Array] = ArrayLikeSeq.untagged

	private def classTag = ClassTag(coll.getClass.getComponentType).asInstanceOf[ClassTag[E]]

	override def equals(that :Any) :Boolean = that match {
		case it :IterableOnce[_] =>
			mutable.ArraySeq.make(coll) == it
		case _ => false
	}
	override def hashCode :Int = mutable.ArraySeq.make(coll).hashCode
	override def toString :String = mutable.ArraySeq.make(coll).mkString("Array(", ", ", ")")
}


object ArrayLikeSeq extends ClassTagIterableFactory[Array] {

	override def from[E :ClassTag](it :IterableOnce[E]) :Array[E] = it match {
		case elems :Iterable[E] if elems.isEmpty => Array.empty[E]
		case iter  :Iterator[E] if !iter.hasNext => Array.empty[E]
		case elems :ArrayLikeSeq[E] => elems.coll
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
			ArrayLikeSeq.from(source)(ClassTag.Any.asInstanceOf[ClassTag[A]])

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

