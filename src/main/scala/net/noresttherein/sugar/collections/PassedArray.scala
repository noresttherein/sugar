package net.noresttherein.sugar.collections

import java.lang.invoke.MethodHandles

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{mutable, Factory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedSeqFactory}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.JavaTypes.{JByte, JChar, JDouble, JFloat, JInt, JIterator, JLong, JShort}
import net.noresttherein.sugar.collections.PassedArrayInternals.{superElementType, InitSize, OwnerField, SliceReallocationFactor}
import net.noresttherein.sugar.reflect.BoxClass
import net.noresttherein.sugar.vars.Opt.Got

//implicits
import net.noresttherein.sugar.extensions.{arrayObjectExtension, castTypeParam, classExtension, iteratorObjectExtension}




/** An array-backed immutable sequence with amortized `O(1)` random access, `slice` and first `append/prepend`.
  * It combines the idea behind the classic growing and shrinking mutable buffer, a dope vector,
  * and passed ownership pattern. It is a view of a section of an array, potentially uninitialized past its bounds.
  * All slicing operations which take above a certain percentage of the whole array are implemented in `O(1)`
  * by passing the same array with new bounds; only when usage drops below a threshold reallocation takes place.
  * Additionally, each instance has a mutable ownership flag, signifying that:
  *   - the array past that instance's bounds is unused by any other instance, and
  *   - it is the only object in existence backed by the same array with that flag set.
  * Whenever new elements are appended/prepended, the instance is the owner of the array, and required space exists
  * before/after its elements, the new elements will be copied to the same array, which will be shared
  * with the created sequence. The ownership flag in that case is passed on to the new instance.
  *
  * In the result, it is actually faster to create a `PassedArray` by appending to an empty instance, or starting
  * with the companion object:
  * [[net.noresttherein.sugar.collections.PassedArray$ PassedArray]]` `[[net.noresttherein.sugar.collections.PassedArray$.:+ :+]]` e1 `[[net.noresttherein.oldsql.collection.PassedArray!.:+ :+]]` e2`.
  *
  * Updates and other operations typically require `O(n)` time.
  *
  * This provides a very fast sequence implementation excelling at operations such as `fold`, `flatMap`
  * and linear recursive processing, as long as the sequences are not too large, leading to problems
  * with memory allocation.
  *
  * Small sequences may have dedicated implementations, without a backing array.
  * @author Marcin Mościcki
  */ //todo: BooleanPassedArray implementation
sealed trait PassedArray[/*@specialized(ElemTypes) */+E]
	extends IndexedSeq[E] with IndexedSeqOps[E, PassedArray, PassedArray[E]]
	   with StrictOptimizedSeqOps[E, PassedArray, PassedArray[E]] with IterableFactoryDefaults[E, PassedArray]
	   with SugaredIterable[E] with SugaredIterableOps[E, PassedArray, PassedArray[E]] with Serializable
{

	override def iterableFactory :SeqFactory[PassedArray] = PassedArray
	protected override def className = "PassedArray"
	private[collections] def elementType :Class[_]

	/** A `PassedArray` being a view on this instance. Equivalent to `slice`, but no new array is allocated
	  * even if the view is a minor fragment of the underlying array (`(until - from)/length` is large).
	  * Additionally, all `slice` operations on the result are also views on this array, without any reallocation.
	  * Adding elements to the view will create a normal, non view instance. This makes `tail` and other
	  * slicing operations very fast, and the implementation particularly suited towards list-like processing.
	  * Due to the possibility of inducing a memory leak, this implementation should be used only by code which
	  * is certain not to expose any of the slices back to the application.
	  *
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collections.PassedArray.safe safe]] creates a copy with normal shrinking behaviour.
	  * The result may be a dedicated class if new size is very small.
	  */
	def window(from :Int, until :Int) :PassedArray[E]

	/** A version of this sequence guarded against memory leaks caused by slicing.
	  * The default implementation is already safe (returns `this`); this method is used in conjunction
	  * with [[net.noresttherein.sugar.collections.PassedArray.window window]], which creates
	  * a view over the underlying array which never copies elements while slicing, regardless of the ratio
	  * between the slice length and the underlying array length.
	  */
	def safe :PassedArray[E] = this

	/** Reallocates (if needed) the data to a new array of an appropriate size to free the larger array
	  * for garbage collection (unless used by another instance).
	  */
	def compact :PassedArray[E] = this

	//specialized
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, PassedArray[E]] =
		PassedArray.newBuilder[E](elementType.asInstanceOf[Class[E]])

	//todo: remove these once the bug is fixed in SeqOps
	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset >= 0 && offset <= length && super.startsWith(that, offset)

	override def indexOfSlice[B >: E](that :collection.Seq[B], from :Int) :Int =
		if (from > length) -1
		else super.indexOfSlice(that, 0 max from)

}



/** An empty `PassedArray`. */
@SerialVersionUID(Ver)
private class PassedArray0 extends PassedArray[Nothing] {
	override def elementType = classOf[AnyRef]
	override def length = 0

	override def head = throw new NoSuchElementException("PassedArray().head")
	override def last = throw new NoSuchElementException("PassedArray().last")

	override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException(i)

	override def iterator = Iterator.empty

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[Nothing, I]) :I =
		JavaIterator.ofInt().asInstanceOf[I]
	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[Nothing, S]) :S with EfficientSplit =
		Stepper0()

	override def window(from :Int, until :Int) :this.type = this
	override def slice(from :Int, until :Int) :this.type = this
	override def take(n :Int) :this.type = this
	override def drop(n :Int) :this.type = this
	override def takeRight(n :Int) :this.type = this
	override def dropRight(n :Int) :this.type = this

	override def updated[B >: Nothing](index :Int, elem :B) :Nothing =
		throw new IndexOutOfBoundsException("PassedArray.empty.updated(" + index + ", " + elem + ")")

	override def appended[B >: Nothing](elem :B) :PassedArray[B] = new PassedArray1(elem)
	override def prepended[B >: Nothing](elem :B) :PassedArray[B] = new PassedArray1(elem)
	override def appendedAll[B >: Nothing](suffix :IterableOnce[B]) :PassedArray[B] = PassedArray.from(suffix)
	override def prependedAll[B >: Nothing](prefix :IterableOnce[B]) :PassedArray[B] = PassedArray.from(prefix)

	override def copyToArray[B >: Nothing](xs :Array[B], start :Int, len :Int) :Int = 0

}



/** A [[net.noresttherein.sugar.collections.PassedArray PassedArray]] with a single element. */
@SerialVersionUID(Ver)
private class PassedArray1[/*@specialized(ElemTypes) */+E] private[collections](override val head :E)
	extends AbstractSeq[E] with PassedArray[E]
{
	override def elementType = BoxClass.unbox(head.getClass)
	override def iterator :Iterator[E] = Iterator.single(head)

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		JavaIterator(head)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		Stepper1(head)

	override def length = 1
	override def apply(i :Int) :E =
		if (i == 0) head else throw new IndexOutOfBoundsException("PassedArray[1](" + i + ")")

	override def last :E = head
	override def tail :PassedArray[E] = PassedArray.empty
	override def init :PassedArray[E] = PassedArray.empty

	override def window(from :Int, until :Int) :PassedArray[E] = slice(from, until)

	override def slice(from :Int, until :Int) :PassedArray[E] =
		if (until <= from | from > 0 | until <= 0) PassedArray.empty else this

	override def take(n :Int) :PassedArray[E] = if (n > 0) this else PassedArray.empty
	override def drop(n :Int) :PassedArray[E] = if (n > 0) PassedArray.empty else this
	override def takeRight(n :Int) :PassedArray[E] = if (n > 0) this else PassedArray.empty
	override def dropRight(n :Int) :PassedArray[E] = if (n > 0) PassedArray.empty else this

	override def takeWhile(p :E => Boolean) :PassedArray[E] = if (p(head)) this else PassedArray.empty
	override def dropWhile(p :E => Boolean) :PassedArray[E] = if (p(head)) PassedArray.empty else this

	override def filter(pred :E => Boolean) :PassedArray[E] = if (pred(head)) this else PassedArray.empty
	override def filterNot(pred :E => Boolean) :PassedArray[E] = if (pred(head)) PassedArray.empty else this
	override def partition(p :E => Boolean) :(PassedArray[E], PassedArray[E]) =
		if (p(head)) (this, PassedArray.empty) else (PassedArray.empty, this)

	override def updated[B >: E](index :Int, elem :B) :PassedArray[B] =
		if (index == 0) //don't use equals unless we know it's cheap
			if (elem.getClass.isBox && elem == head) this else PassedArray.single(elem)
		else
			throw new IndexOutOfBoundsException("PassedArray[1].updated(" + index + "," + elem + ")")

	//extracted for specialization.
	private[this] def seq2(elem1 :E, elem2 :E) = new PassedArray2(elem1, elem2)

	private[this] def large(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)

	override def appended[B >: E](elem :B) :PassedArray[B] =
		try { seq2(head, elem.asInstanceOf[E]) } catch {
			case _ :ClassCastException => new PassedArray2(head, elem)
		}
	override def prepended[B >: E](elem :B) :PassedArray[B] =
		try { seq2(elem.asInstanceOf[E], head) } catch {
			case _ :ClassCastException => new PassedArray2(elem, head)
		}


	override def appendedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] = suffix match {
		case items :Iterable[B] if items.isEmpty => this
		case items :Iterator[B] if !items.hasNext => this
		case items :PassedArray1[B] => try {
			seq2(head, items.head.asInstanceOf[E])
		} catch {
			case _ :ClassCastException =>
				val elem = items.head
				new PassedArray2[B](head, elem)
		}
		case items :PassedArray[B] => this ++: items
		case items :Iterable[B] => items.knownSize match {
			case 1 => try {
				seq2(head, items.head.asInstanceOf[E])
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new PassedArray2(head, elem)
			}
			case n => appendedAllLarge(items, n)
		}
		case _ =>
			val i = suffix.iterator
			i.knownSize match {
				case 1 => try {
					seq2(head, i.next().asInstanceOf[E])
				} catch {
					case _ :ClassCastException =>
						val elem = i.next()
						new PassedArray2[B](head, elem)
				}
				case n => appendedAllLarge(i, n)
			}
	}

	private def appendedAllLarge[B >: E](items :IterableOnce[B], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder += head ++= items.asInstanceOf[Iterable[E]]).result()
			} catch {
				case _ :ClassCastException => (PassedArray.genericBuilder[B] += head ++= items).result()
			}
			case  n => try {
				val capacity = Math.max(n + 1, InitSize)
				val array = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
				items match {
					case it :Iterable[E @unchecked] => it.copyToArray(array, 1)
					case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, 1)
				}
				array(0) = head
				large(array, 0, 1 + n)
			} catch {
				case _ :ClassCastException =>
					val array = new Array[AnyRef](Math.max(1 + n, InitSize)).asInstanceOf[Array[B]]
					items match {
						case it :Iterable[B] => it.copyToArray(array, 1)
						case _ => items.iterator.copyToArray(array, 1)
					}
					array(0) = head
					new PassedArrayPlus(array, 0, 1 + n, true)
			}
		}


	override def prependedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] = suffix match {
		case items :Iterable[B] if items.isEmpty => this
		case items :Iterator[B] if !items.hasNext => this
		case items :PassedArray1[B] => try {
			seq2(items.head.asInstanceOf[E], head)
		} catch {
			case _ :ClassCastException =>
				new PassedArray2[B](items.head, head)
		}
		case items :PassedArray[B] => items :++ this
		case items :Iterable[B] => items.knownSize match {
			case 1 => try {
				seq2(items.head.asInstanceOf[E], head)
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new PassedArray2(elem, head)
			}
			case n => prependedAllLarge(items, n)
		}
		case _ =>
			val i = suffix.iterator
			i.knownSize match {
				case 1 => try {
					seq2(i.next().asInstanceOf[E], head)
				} catch {
					case _ :ClassCastException =>
						val elem = i.next()
						new PassedArray2[B](elem, head)
				}
				case n => prependedAllLarge(i, n)
			}
	}

	private def prependedAllLarge[B >: E](items :IterableOnce[B], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder ++= items.asInstanceOf[Iterable[E]] += head).result()
			} catch {
				case _ :ClassCastException => (PassedArray.genericBuilder[B] ++= items += head).result()
			}
			case  n => try {
				val newSize = n + 1
				val capacity = Math.max(newSize, InitSize)
				val array = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
				items match {
					case it :Iterable[E @unchecked] => it.copyToArray(array, capacity - newSize)
					case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, capacity - newSize)
				}
				array(capacity - 1) = head
				large(array, capacity - newSize, newSize)
			} catch {
				case _ :ClassCastException =>
					val newSize = n + 1
					val capacity = Math.max(newSize, InitSize)
					val array = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
					items match {
						case it :Iterable[B] => it.copyToArray(array, capacity - newSize)
						case _ => items.iterator.copyToArray(array, capacity - newSize)
					}
					array(capacity - 1) = head
					new PassedArrayPlus(array, capacity - newSize, newSize, true)
			}
		}


	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (start >= xs.length | len <= 0)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start)
		else if (start >= xs.length)
			0
		else try {
			copy(xs.asInstanceOf[Array[E]], start); 1
		} catch {
			case _ :ClassCastException => xs(start) = head; 1
		}

	private def copy(xs :Array[E @uncheckedVariance], start :Int) :Unit = xs(start) = head
}




/** A [[net.noresttherein.sugar.collections.PassedArray PassedArray]] of two elements. */
@SerialVersionUID(Ver)
private final class PassedArray2[/*@specialized(Specializable.Arg) */+E] private[collections]
	                            (override val head :E, override val last :E, override val elementType :Class[_])
	extends AbstractSeq[E] with PassedArray[E]
{
	def this(head :E, last :E) = this(head, last, superElementType(head, last))
	override def iterator = Iterator.double(head, last)

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		JavaIterator(head, last)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		Stepper2(head, last)

	override def length = 2

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => last
		case _ => throw new IndexOutOfBoundsException("PassedArray[2](" + i + ")")
	}

	override def tail :PassedArray[E] = new PassedArray1(last)
	override def init :PassedArray[E] = new PassedArray1(head)

	override def window(from :Int, until :Int) :PassedArray[E] = slice(from, until)

	override def slice(from :Int, until :Int) :PassedArray[E] =
		if (until <= from | from > 1 | until <= 0) PassedArray.empty
		else if (until == 1) new PassedArray1(head)
		else if (from == 1) new PassedArray1(last)
		else this

	override def take(n :Int) :PassedArray[E] =
		if (n >= 2) this
		else if (n == 1) new PassedArray1(head)
		else PassedArray.empty

	override def drop(n :Int) :PassedArray[E] =
		if (n >= 2) PassedArray.empty
		else if (n == 1) new PassedArray1(last)
		else this

	override def takeRight(n :Int) :PassedArray[E] =
		if (n >= 2) this
		else if (n == 1) new PassedArray1(last)
		else PassedArray.empty

	override def dropRight(n :Int) :PassedArray[E] =
		if (n >= 2) PassedArray.empty
		else if (n == 1) new PassedArray1(head)
		else this

	override def takeWhile(p :E => Boolean) :PassedArray[E] =
		if (p(head))
			if (p(last)) this else new PassedArray1(head)
		else
			PassedArray.empty

	override def dropWhile(p :E => Boolean) :PassedArray[E] =
		if (p(head))
			if (p(last)) PassedArray.empty else new PassedArray1(last)
		else
			this

	override def filter(pred :E => Boolean) :PassedArray[E] =
		if (pred(head))
			if (pred(last)) this else new PassedArray1(head)
		else
			if (pred(last)) new PassedArray1(last) else PassedArray.empty

	override def filterNot(pred :E => Boolean) :PassedArray[E] =
		if (pred(head))
			if (pred(last)) PassedArray.empty else new PassedArray1(last)
		else
			if (pred(last)) new PassedArray1(head) else this

	override def partition(p :E => Boolean) :(PassedArray[E], PassedArray[E]) =
		if (p(head))
			if (p(last)) (this, PassedArray.empty) else (new PassedArray1(head), new PassedArray1(last))
		else
			if (p(last)) (new PassedArray1(last), new PassedArray1(head)) else (PassedArray.empty, this)


	override def updated[B >: E](index :Int, elem :B) :PassedArray[B] = index match {
		//equals may be expensive
		case 0 if (elementType.isPrimitive || elementType.isBox) && elem == head => this
		case 0 => new PassedArray2(elem, last)
		case 1 if (elementType.isPrimitive || elementType.isBox) && elem == last => this
		case 1 => new PassedArray2(head, elem)
		case _ => throw new IndexOutOfBoundsException(toString + ".updated(" + index + ", " + elem + ")")
	}

	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)

	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val isSubtype = BoxClass.unbox(elementType) isAssignableFrom BoxClass.unbox(elem.getClass)
		val tpe = if (isSubtype) elementType else classOf[Any].asInstanceOf[Class[E]]
		val copy = java.lang.reflect.Array.newInstance(tpe, InitSize).asInstanceOf[Array[E]]
		copy(2) = elem.asInstanceOf[E]
		copyToArray(copy, 0, 2)
		new PassedArrayPlus[E](copy, 0, 3, true)
	}

	override def prepended[B >: E](elem :B) :PassedArray[B] = {
		val isSubtype = BoxClass.unbox(elementType) isAssignableFrom BoxClass.unbox(elem.getClass)
		val tpe = if (isSubtype) elementType else classOf[Any].asInstanceOf[Class[E]]
		val copy = java.lang.reflect.Array.newInstance(tpe, InitSize).asInstanceOf[Array[E]]
		val newOffset = InitSize - 3
		copy(newOffset) = elem.asInstanceOf[E]
		copyToArray(copy, InitSize - 2, 2)
		new PassedArrayPlus(copy, newOffset, 3, true)
	}


	override def appendedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] =
		suffix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] =>
				if (elementType isAssignableFrom it.elementType)
					appendedAll(it.asInstanceOf[PassedArray[E]])
				else {
					val newSize = it.length + 2
					val capacity = Math.max(newSize, InitSize)
					val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[B]]
					copy(0) = head
					copy(1) = last
					it.copyToArray(copy, 2)
					new PassedArrayPlus(copy, 0, newSize, true)
				}
			case it :Iterable[B] => it.knownSize match {
				case -1 => super.appendedAll(it)
				case  n => try {
					val capacity = Math.max(n + 2, InitSize)
					val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
					write(copy, 0, 2)
					it.copyToArray(copy.asInstanceOf[Array[B]], 2)
					newInstance(copy, 0, n + 2)
				} catch {
					case _ :ClassCastException =>
						val capacity = Math.max(n + 2, InitSize)
						val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
						copy(0) = head
						copy(1) = last
						it.copyToArray(copy, 2, n + 2)
						new PassedArrayPlus(copy, 0, n + 2, true)
				}
			}
			case _ => super.appendedAll(suffix)
		}

	private[this] def appendedAll(suffix :PassedArray[E]) =
		if (suffix.isEmpty) this
		else {
			val newSize = suffix.length + 2
			val capacity = Math.max(newSize, InitSize)
			val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
			copy(0) = head
			copy(1) = last
			suffix.copyToArray(copy, 2)
			new PassedArrayPlus(copy, 0, newSize, true)
		}


	override def prependedAll[B >: E](prefix :IterableOnce[B]) :PassedArray[B] =
		prefix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] =>
				if (elementType isAssignableFrom it.elementType)
					prependedAll(it.asInstanceOf[PassedArray[E]])
				else {
					val newSize = it.length + 2
					val capacity = Math.max(newSize, InitSize)
					val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[B]]
					copy(capacity - 2) = head
					copy(capacity - 1) = last
					it.copyToArray(copy, capacity - newSize)
					new PassedArrayPlus(copy, capacity - newSize, newSize, true)
				}
			case it :Iterable[B] => it.knownSize match {
				case -1 => super.prependedAll(it)
				case  n => try {
					val capacity = Math.max(n + 2, InitSize)
					val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
					write(copy, capacity - 2, 2)
					val newOffset = capacity - 2 - n
					it.copyToArray(copy.asInstanceOf[Array[B]], newOffset, n)
					newInstance(copy, newOffset, n + 2)
				} catch {
					case _ :ClassCastException =>
						val capacity = Math.max(n + 2, InitSize)
						val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
						copy(capacity - 2) = head
						copy(capacity - 1) = last
						val newOffset = capacity - 2 - n
						it.copyToArray(copy, newOffset, n)
						new PassedArrayPlus[B](copy, newOffset, n + 2, true)
				}
			}
			case _ => super.prependedAll(prefix)
		}

	private[this] def prependedAll(suffix :PassedArray[E]) =
		if (suffix.isEmpty) this
		else {
			val newSize = suffix.length + 2
			val capacity = Math.max(newSize, InitSize)
			val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
			copy(0) = head
			copy(1) = last
			suffix.copyToArray(copy, 2)
			new PassedArrayPlus(copy, 0, newSize, true)
		}


	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (start >= xs.length | len <= 0)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start)
		else {
			val copied = Math.min(2, Math.min(xs.length - start, len))
			try {
				val xsType = xs.getClass.getComponentType
				if (xsType.isAssignableFrom(elementType))
					write(xs.asInstanceOf[Array[E]], start, copied)
				else {
					xs(start) = head
					if (copied > 1)
						xs(start + 1) = last
				}
			} catch {
				case _ :ClassCastException =>
					xs(start) = head
					if (copied > 1)
						xs(start + 1) = last
			}
			copied
		}

	private[this] def write(xs :Array[E], start :Int, len :Int) :Unit = {
		xs(start) = head
		if (len > 1)
			xs(start + 1) = last
	}

}




/** Most of the implementation of an actual array backed [[net.noresttherein.sugar.collections.PassedArray PassedArray]].
  * Base trait for [[net.noresttherein.sugar.collections.PassedArrayPlus PassedArrayPlus]]
  * and [[net.noresttherein.sugar.collections.PassedArrayView PassedArrayView]].
  */
private[collections] sealed trait AbstractPassedArray[/*@specialized(ElemTypes) */+E] extends PassedArray[E] {
	override def elementType :Class[_] = elems.getClass.getComponentType
	private[collections] def elems[U >: E] :Array[U]
	private[collections] def startIndex :Int

	override def updated[B >: E](index :Int, elem :B) :PassedArray[B] = {
		val len = length
		if (index < 0 | index >= len)
			throw new IndexOutOfBoundsException(index.toString + " out of " + len)
		val array = elems
		val offset = startIndex
		val elemType = elementType
		if ((elemType.isPrimitive || elemType.isBox) && array(offset + index) == elem)
			this
		else {
			val res =
				if (elemType isAssignableFrom elem.getClass) PassedArray.newBuilder(elemType.castParam[B])
				else PassedArray.newBuilder[B]
			res sizeHint len + 1
			if (index > 0)
				res ++= new PassedArrayPlus(array, offset, index)
			res += elem
			if (index < len - 1)
				res ++= new PassedArrayPlus(array, offset + index + 1, len - index - 1)
			res.result()
		}
	}

	override def iterator :Iterator[E] = new ArrayIterator[E](elems, startIndex, startIndex + length)

	override def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		stepper(shape.stepperShape).javaIterator.asInstanceOf[I]

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		ArrayStepper(elems, startIndex, startIndex + length)

	override def window(from :Int, until :Int) :PassedArray[E] = {
		val offset = startIndex; val len = length
		if (until <= from | until <= 0 | from >= len)
			PassedArray.empty
		else if (from <= 0 && until >= len)
			this
		else {
			val array   = elems
			val start   = Math.max(from, 0)
			val newSize = Math.min(len, until) - start
			if (newSize == 1)
				new PassedArray1(array(offset + start))
			else if (newSize == 2)
				new PassedArray2(array(offset + start), array(offset + start + 1), elementType)
			else
				new PassedArrayView(array, offset + start, newSize)
		}
	}


	override def take(n :Int) :PassedArray[E] = slice(0, n)
	override def drop(n :Int) :PassedArray[E] = slice(n, length)

	override def takeRight(n :Int) :PassedArray[E] =
		if (n <= 0) PassedArray.empty
		else slice(length - n, length)

	override def dropRight(n :Int) :PassedArray[E] =
		if (n <= 0) this
		else slice(0, length - n)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
		if (start >= xs.length | len <= 0)
			0
		else if (start < 0)
			throw new IndexOutOfBoundsException(start)
		else {
			val copied = Math.min(length, Math.min(len, xs.length - start))
			Array.copy(elems, startIndex, xs, start, copied)
			copied
		}


	override def compact :PassedArray[E] =
		if (elems.length == length)
			this
		else {
			val copy = elems.slice(startIndex, startIndex + length)
			new PassedArrayPlus(copy)
		}

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Got(PassedArray) | Got(Seq) | Got(IndexedSeq) | Got(collection.Seq) | Got(collection.IndexedSeq) =>
			this.asInstanceOf[C1]
		case Got(ArraySeq) if length == elems.length =>
			ArraySeq.unsafeWrapArray(elems).asInstanceOf[C1]
		case _ => super.to(factory)
	}

}




/** The main, truly array backed, implementation of [[net.noresttherein.sugar.collections.PassedArray PassedArray]].
  * Primarily, it is a sequence of elements stored in a chosen section of an array.
  * Carries an ownership flag `owner`, with an invariant that only one object on the whole heap
  * among those sharing the same array has this flag set. On first append/prepend, if the new elements
  * will fit in the array after/before the elements of this collection, they conform to the array element type
  * and this collection is the owner of the array, no new array is allocated and the elements are written
  * in the underlying one at appropriate positions. The `owner` flag is atomically passed to the freshly created
  * collection. In case reallocation is needed, the new array will have `max(this.length/2, newElements.size)`
  * free cells following/preceding the elements copied from this array. If the unused (by this instance) array section
  * on the opposite size exceeds `this.length/2`, it is reduced to that number.
  * All slicing operations return instances backed by the same array, unless their length
  * is less than 1/4 of the underlying length (not this sequence's length).
  *
  * The class is `@specialized` and in most, but not necessarily all cases the array element type
  * equals the specialization type. All reference types are normally (although not strictly enforced)
  * stored in an `Array[AnyRef]`, so as to maximize the likelihood that append/prepend
  * will be able to use the free space.
  * @see [[net.noresttherein.sugar.collections.PassedArrayView PassedArrayView]]
  */
@SerialVersionUID(Ver)
private final class PassedArrayPlus[/*@specialized(ElemTypes) */+E] private[collections]
	                               (array :Array[E], offset :Int, len :Int, owner :Boolean = false)
	extends AbstractSeq[E] with AbstractPassedArray[E]
{
	private[collections] def this(array :Array[E]) = this(array, 0, array.length)

	assert(offset >= 0, "negative offset " + offset + " out of " + array.length)
	assert(len >= 0, "negative length " + len)
	assert(len <= array.length - offset, "array[" + array.length + "].slice(" + offset + ", " + (offset + len) + ")")

	private[collections] override def elems[U >: E] :Array[U] = array.asInstanceOf[Array[U]]
	private[collections] override def startIndex :Int = offset
	@volatile private[this] var isOwner = owner

	private def canPassOn :Boolean = {
		var read :Boolean = isOwner
		while (read && !OwnerField.weakCompareAndSet(this, true, false))
			read = isOwner
		read
	}

	override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			throw new IndexOutOfBoundsException("PassedArray[" + len + "](" + i + ")")
		else
			array(offset + i)

	override def slice(from :Int, until :Int) :PassedArray[E] =
		if (until <= from | until <= 0 | from >= len)
			PassedArray.empty
		else if (from <= 0 & until >= len)
			this
		else {
			val start   = Math.max(from, 0)
			val newSize = Math.min(len, until) - start
			newSize match {
				case 1 =>
					new PassedArray1(array(offset + start))
				case 2 =>
					new PassedArray2(array(offset + start), array(offset + start + 1), elementType)
				case _ if newSize < array.length / SliceReallocationFactor =>
					val copy = java.lang.reflect.Array.newInstance(elementType, newSize).asInstanceOf[Array[E]]
					Array.copy(array, offset + start, copy, 0, newSize)
					new PassedArrayPlus(copy)
				case _ =>
					new PassedArrayPlus(array, offset + start, newSize)
			}
		}


	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)

	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore =
			elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType) || elementType.isBoxOf(elemType)
		var res :PassedArray[B] = null
		if (canStore)
			if (offset + len < array.length) {
				if (canPassOn) {
					array(offset + len) = elem.asInstanceOf[E]
					res = newInstance(array, offset, len + 1)
				}
			} else if (len == 0 && array.length > 0 && canPassOn) {
				array(0) = elem.asInstanceOf[E]
				res = newInstance(array, 0, 1)
			}
		if (res == null) {
			val halfSize = (len + 1) / 2
			val newOffset = Math.min(offset, halfSize)
			val capacity = Math.max(newOffset + len + halfSize, InitSize)
			val copy =
				if (canStore) java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[E]]
			copy(newOffset + len) = elem.asInstanceOf[E]
			Array.copy(array, offset, copy, newOffset, len)
			res =
				if (canStore) newInstance(copy, newOffset, len + 1)
				else new PassedArrayPlus(copy, newOffset, len + 1, true)
		}
		res
	}


	override def prepended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore =
			elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType) || elementType.isBoxOf(elemType)
		var res :PassedArray[B] = null
		if (canStore)
			if (offset > 0) {
				if (canPassOn) {
					array(offset - 1) = elem.asInstanceOf[E]
					res = newInstance(array, offset - 1, len + 1)
				}
			} else if (len == 0 && array.length > 0 && canPassOn) {
				val newOffset = array.length - 1
				array(newOffset) = elem.asInstanceOf[E]
				res = newInstance(array, newOffset, 1)
			}
		if (res == null) {
			val halfSize = (len + 1) / 2
			val padding = Math.min(array.length - offset - len, halfSize)
			val capacity = Math.max(halfSize + len + padding, InitSize)
			val newOffset = capacity - padding - len
			val copy =
				if (canStore) java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[E]]
			Array.copy(array, offset, copy, newOffset, len)
			copy(newOffset - 1) = elem.asInstanceOf[E]
			res =
				if (canStore) newInstance(copy, newOffset - 1, len + 1)
				else new PassedArrayPlus(copy, newOffset - 1, len + 1)
		}
		res
	}

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] =
		suffix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] if len == 0 && (it.length > array.length || !isOwner) => it
			case it :PassedArray[B] =>
				val capacity  = array.length
				val extras    = it.length
				val newSize   = len + extras
				val myType    = elementType
				val theirType = it.elementType
				val canStore  =
					myType.isAssignableFrom(theirType) || myType.isBoxOf(theirType) || theirType.isBoxOf(myType)
				var res :PassedArray[B] = null
				if (canStore)
					if (offset + newSize <= capacity) {
						if (canPassOn) {
							it.copyToArray(array.asInstanceOf[Array[B]], offset + len, extras)
							res = newInstance(array, offset, newSize)
						}
					} else if (len == 0 && extras <= capacity && canPassOn) {
						it.copyToArray(array.asInstanceOf[Array[B]], 0, extras)
						res = newInstance(array, 0, extras)
					}
				if (res == null) {
					val halfSize = (len + 1) / 2
					val newOffset = Math.min(offset, halfSize)
					val capacity = Math.max(newOffset + len + Math.max(extras, halfSize), InitSize)
					val copy =
						if (canStore) java.lang.reflect.Array.newInstance(myType, capacity).asInstanceOf[Array[B]]
						else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
					Array.copy(array, offset, copy, newOffset, len)
					it.copyToArray(copy, newOffset + len, extras)
					if (canStore)
						res = newInstance(copy.asInstanceOf[Array[E]], newOffset, newSize)
					else
						res = new PassedArrayPlus(copy, newOffset, newSize, true)
				}
				res
			case it :Iterable[B] => it.knownSize match {
				case -1 =>
					super.appendedAll(it)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :PassedArray[B] = null
						if (offset + newSize <= elems.length) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(array.asInstanceOf[Array[B]], offset + len, newSize)
								res = newInstance(array, offset, newSize)
							}
						}
						if (res == null) {
							val halfSize = (len + 1) / 2
							val newOffset = Math.min(offset, halfSize)
							val capacity = Math.max(newOffset + len + Math.max(extras, halfSize), InitSize)
							val copy = java.lang.reflect.Array.newInstance(
								array.getClass.getComponentType, capacity
							).asInstanceOf[Array[E]]
							it.copyToArray(copy.asInstanceOf[Array[B]], newOffset + len, newSize)
							Array.copy(array, offset, copy, newOffset, len)
							res = newInstance(copy, newOffset, newSize)
						}
						res
					} catch {
						case _ :ClassCastException =>
							if (wasOwner)
								isOwner = true
							val halfSize = (len + 1) / 2
							val newOffset = Math.min(offset, halfSize)
							val capacity = Math.max(newOffset + len + Math.max(extras, halfSize), InitSize)
							val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
							it.copyToArray(copy, newOffset + len, newSize)
							Array.copy(elems, offset, copy, newOffset, len)
							new PassedArrayPlus(copy, newOffset, newSize, true)
					}
			}
			case _ => super.appendedAll(suffix)
		}


	override def prependedAll[B >: E](prefix :IterableOnce[B]) :PassedArray[B] =
		prefix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] if len == 0 && (it.length > array.length || !isOwner) => it
			case it :PassedArray[B] =>
				val extras    = it.length
				val newSize   = len + extras
				val myType    = elementType
				val theirType = it.elementType
				val canStore  =
					myType.isAssignableFrom(theirType) || myType.isBoxOf(theirType) || theirType.isBoxOf(myType)
				var res :PassedArray[B] = null
				if (canStore)
					if (offset >= extras) {
						if (canPassOn) {
							it.copyToArray(array.asInstanceOf[Array[B]], offset - extras, extras)
							res = newInstance(array, offset - extras, newSize)
						}
					} else if (len == 0 && extras <= array.length && canPassOn) {
						val newOffset = array.length - extras
						it.copyToArray(array.asInstanceOf[Array[B]], newOffset, extras)
						res = newInstance(array, newOffset, extras)
					}
				if (res == null) {
					val halfSize = (len + 1) / 2
					val padding = Math.min(elems.length - offset - len, halfSize)
					val capacity = Math.max(Math.max(extras, halfSize) + len + padding, InitSize)
					val newOffset = capacity - padding - len - extras
					val copy =
						if (canStore) java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[B]]
						else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
					Array.copy(elems, offset, copy, newOffset + extras, len)
					it.copyToArray(copy, newOffset, extras)
					if (canStore)
						res = newInstance(copy.asInstanceOf[Array[E]], newOffset, newSize)
					else
						res = new PassedArrayPlus[B](copy, newOffset, newSize, true)
				}
				res
			case it :Iterable[B] => it.knownSize match {
				case -1 => super.prependedAll(prefix)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :PassedArray[B] = null
						if (offset >= extras) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(array.asInstanceOf[Array[B]], offset - extras, extras)
								res = newInstance(array, offset - extras, newSize)
							}
						}
						if (res == null) {
							val halfSize = (len + 1) / 2
							val padding = Math.min(array.length - offset - len, halfSize)
							val capacity = Math.max(Math.max(extras, halfSize) + len + padding, InitSize)
							val newOffset = capacity - padding - len - extras
							val copy = java.lang.reflect.Array.newInstance(
								array.getClass.getComponentType, capacity
							).asInstanceOf[Array[E]]
							it.copyToArray(copy.asInstanceOf[Array[B]], newOffset, extras)
							Array.copy(array, offset, copy, newOffset + extras, len)
							res = newInstance(copy, newOffset, newSize)
						}
						res
					} catch {
						case _ :ClassCastException =>
							if (wasOwner) {
								isOwner = true
							}
							val halfSize = (len + 1) / 2
							val padding = Math.min(elems.length - offset - len, halfSize)
							val capacity = Math.max(Math.max(extras, halfSize) + len + padding, InitSize)
							val newOffset = capacity - padding - len - extras
							val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
							it.copyToArray(copy, newOffset, extras)
							Array.copy(array, offset, copy, newOffset + extras, len)
							new PassedArrayPlus(copy, newOffset, newSize, true)
					}
			}
			case _ => super.prependedAll(prefix)
		}

	private def writeReplace = new Serializable {
		private[this] val data = if (array.length == len) array else array.slice(offset, offset + length)
		private def readResolve = new PassedArrayPlus(data)
	}

	private def fieldHandle =
		MethodHandles.lookup().findVarHandle(classOf[PassedArrayPlus[Any]],
			"isOwner", classOf[Boolean]
//			classOf[PassedArrayPlus[Any]].getName.replace('.', '$') + "$$isOwner", classOf[Boolean]
		)

}




/** An array backed implementation of [[net.noresttherein.sugar.collections.PassedArray PassedArray]]
  * which is not the owner of the underlying array (hence all appends and prepends will allocate a new array).
  * Additionally, slicing operations ''always'' return a view over the same array instance, never reallocating
  * the elements (unlike in [[net.noresttherein.sugar.collections.PassedArrayPlus PassedArrayPlus]].
  */
@SerialVersionUID(Ver)
private final class PassedArrayView[@specialized(ElemTypes) +E] private[collections]
	                               (array :Array[E], offset :Int, len :Int)
	extends AbstractSeq[E] with AbstractPassedArray[E]
{
	private[collections] override def elems[U >: E] :Array[U] = array.asInstanceOf[Array[U]]
	private[collections] override def startIndex :Int = offset
	override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			throw new IndexOutOfBoundsException("PassedArray[" + len + "](" + i + ")")
		else
			array(offset + i)

	override def safe :PassedArray[E] = {
		val copy = java.lang.reflect.Array.newInstance(elementType, len).asInstanceOf[Array[E]]
		Array.copy(array, offset, copy, 0, len)
		new PassedArrayPlus(array, 0, len)
	}

	override def slice(from :Int, until :Int) :PassedArray[E] = window(from, until)

	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def passedArray(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)


	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore =
			elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType) || elementType.isBoxOf(elemType)
		if (canStore) {
			val copy = java.lang.reflect.Array.newInstance(elementType, len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			Array.copy(array, offset, copy, 0, len)
			passedArray(copy, 0, len + 1)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			Array.copy(array, offset, copy, 0, len)
			new PassedArrayPlus(copy, 0, len + 1)
		}
	}


	override def prepended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore =
			elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType) || elementType.isBoxOf(elemType)
		if (canStore) {
			val copy = java.lang.reflect.Array.newInstance(elementType, len + 1).asInstanceOf[Array[E]]
			Array.copy(array, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			passedArray(copy, 0, len + 1)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			Array.copy(array, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			new PassedArrayPlus(copy, 0, len + 1)
		}
	}

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] =
		suffix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] if len == 0 => it
			case it :PassedArray[B] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  =
					myType.isAssignableFrom(theirType) || myType.isBoxOf(theirType) || theirType.isBoxOf(myType)
				if (canStore) {
					val copy = java.lang.reflect.Array.newInstance(myType, len + extras).asInstanceOf[Array[B]]
					Array.copy(array, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					passedArray(copy.asInstanceOf[Array[E]], 0, copy.length)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[B]]
					Array.copy(array, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					new PassedArrayPlus(copy, 0, copy.length)
				}
			case _ => super.appendedAll(suffix)
		}


	override def prependedAll[B >: E](prefix :IterableOnce[B]) :PassedArray[B] =
		prefix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :PassedArray[B] if len == 0 => it
			case it :PassedArray[B] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  =
					myType.isAssignableFrom(theirType) || myType.isBoxOf(theirType) || theirType.isBoxOf(myType)
				if (canStore) {
					val copy = java.lang.reflect.Array.newInstance(elementType, len + extras).asInstanceOf[Array[B]]
					Array.copy(elems, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					passedArray(copy.asInstanceOf[Array[E]], 0, len + extras)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[B]]
					Array.copy(elems, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					new PassedArrayPlus(copy.asInstanceOf[Array[E]], 0, len + extras)
				}
			case _ => super.prependedAll(prefix)
		}


	private def writeReplace = new Serializable {
		private[this] val data = if (length == array.length) array else array.slice(offset, offset + length)
		private def readResolve = new PassedArrayPlus(data)
	}
}






/** $factoryInfo
  *
  * Despite $Coll being backed by an array,
  * this is not an [[scala.collection.EvidenceIterableFactory EvidenceIterableFactory]].
  * Instead, it takes an optimistic approach to specialization: if elements added to a `PassedArray`, its builder,
  * or passed as arguments to any of factory methods of this object are all of a built in value type,
  * a proper primitive array is allocated. At the same time, it guards itself against type errors which would
  * result in a [[ClassCastException]], and reallocates the underlying array as an `Array[`[[AnyRef]]`]` if needed.
  * @define Coll `PassedArray`
  * @define coll passed array
  */
@SerialVersionUID(Ver)
case object PassedArray extends StrictOptimizedSeqFactory[PassedArray] {
//	def apply[A](elems :A*)(implicit elemType :Maybe[ClassTag[A]]) :PassedArray[A] =
//		elemType.opt match {
//			case _ if elems.isEmpty => empty
//			case Got(tpe) => new PassedArrayPlus[A](elems.toArray[A](tpe))
//			case _ => from(elems)
//		}

	override def from[A](it :IterableOnce[A]) :PassedArray[A] = it match {
		case elems :PassedArray[A] =>
			elems
		case elems :Iterable[A] if elems.isEmpty =>
			Empty
		case elems :ArraySeq[A] =>
			new PassedArrayPlus(elems.unsafeArray.asInstanceOf[Array[A]])
		case elems :mutable.ArraySeq[A] =>
			new PassedArrayPlus(Array.copyOf(elems.array, elems.array.length).asInstanceOf[Array[A]])
		case elems :Iterable[A] => elems.head.getClass match {
			case BoxClass(box) => try {
				new PassedArrayPlus(elems.toArray(ClassTag(box).castParam[A]))
			} catch {
				case _ :ClassCastException =>
					new PassedArrayPlus(elems.toArray(ClassTag(classOf[AnyRef]).castParam[A]))
			}
			case _ =>
				new PassedArrayPlus(elems.toArray(ClassTag(classOf[AnyRef]).castParam[A]))
		}
		case _ =>
			val i = it.iterator
			if (!i.hasNext) Empty
			else new PassedArrayPlus(i.toArray(classTag[AnyRef].asInstanceOf[ClassTag[A]]))
	}

	implicit def from[A](array :IArray[A]) :PassedArray[A] = new PassedArrayPlus(array.asInstanceOf[Array[A]])

	override def empty[A] :PassedArray[A] = Empty

	@inline def :+[A](elem :A) :PassedArray[A] = single(elem)

	@inline def :++[A](elems :IterableOnce[A]) :PassedArray[A] = from(elems)

	/** A $Coll singleton */
	def single[A](elem :A) :PassedArray[A] = new PassedArray1(elem)

	/** A $Coll singleton (same as [[net.noresttherein.sugar.collections.PassedArray.single single]]). */
	def one[@specialized(ElemTypes) A](elem :A) :PassedArray[A] = single(elem)

	/** A special $Coll of only two elements. */
	def two[@specialized(ElemTypes) A](first :A, second :A) :PassedArray[A] = {
		val firstClass = first.getClass
		val secondClass = second.getClass
		if (firstClass != secondClass)
			new PassedArray2(first, second)
		else //only for as long as it is specialized
			(if (firstClass == classOf[JInt])
				new PassedArray2(first.asInstanceOf[Int], second.asInstanceOf[Int], classOf[Int])
			else if (firstClass == classOf[JLong])
				new PassedArray2(first.asInstanceOf[Long], second.asInstanceOf[Long], classOf[Long])
			else if (firstClass == classOf[JDouble])
				new PassedArray2(first.asInstanceOf[Double], second.asInstanceOf[Double], classOf[Double])
			else if (firstClass == classOf[JByte])
				new PassedArray2(first.asInstanceOf[Byte], second.asInstanceOf[Byte], classOf[Byte])
			else if (firstClass == classOf[JChar])
				new PassedArray2(first.asInstanceOf[Char], second.asInstanceOf[Char], classOf[Char])
			else if (firstClass == classOf[JFloat])
				new PassedArray2(first.asInstanceOf[Float], second.asInstanceOf[Float], classOf[Float])
			else if (firstClass == classOf[JShort])
				new PassedArray2(first.asInstanceOf[Short], second.asInstanceOf[Short], classOf[Short])
			else
				new PassedArray2(first, second, classOf[Any])
			).castParam[A]
	}

	/** An empty $Coll backed by the array of the specified length. Similarly to the capacity argument
	  * to [[scala.collection.mutable.ArrayBuffer ArrayBuffer]] and [[StringBuilder]], it allows to avoid
	  * repeated reallocations if the future size is known, at least approximately. Each initial append/prepend
	  * will create only a lightweight object, backed by the array created here, for as long as its capacity
	  * is not exceeded.
	  */
	def ofCapacity[A](size :Int) :PassedArray[A] = size match {
		case 0 => empty
		case n => new PassedArrayPlus[A](new Array[Any](n).castParam[A], 0, 0, true)
	}

	/** An optimistic `PassedArray` builder; if the first element added is a value type (or, rather, a Java box
	  * for a Java primitive type), it allocates an array of the appropriate value type. The classes of subsequently
	  * added elements are checked until a first element with a type not fitting in the array is found,
	  * in which case the underlying array is reallocated as `Array[AnyRef]`.
	  * It is somewhat slower than [[net.noresttherein.sugar.collections.PassedArray.genericBuilder genericBuilder]],
	  * especially when adding individual elements, but considerably faster when adding `Array`-backed collections:
	  * [[collection.immutable.ArraySeq immutable.ArraySeq]], [[collection.mutable.ArraySeq mutable.ArraySeq]]
	  * and `PassedArray`.
	  */
	override def newBuilder[A] :Builder[A, PassedArray[A]] = //newBuilder(classOf[AnyRef].asInstanceOf[Class[A]])
		new OptimisticBuilder[A](null)

	/** A builder of a `PassedArray` backed by an `Array[AnyRef]`. More efficient for reference types than
	  * [[net.noresttherein.sugar.collections.PassedArray.newBuilder newBuilder]].
	  */
	def genericBuilder[A] :Builder[A, PassedArray[A]] =
		newBuilder(classOf[AnyRef].castParam[A])

	/** A builder for a $Coll backed by an array with a specific element type, defined by the implicit [[ClassTag]]. */
	def specificBuilder[A :ClassTag] :Builder[A, PassedArray[A]] =
		newBuilder(classTag[A].runtimeClass.asInstanceOf[Class[A]])

	/** A builder for a $Coll backed by an array with the specified element type. */
	def newBuilder[A](elementType :Class[A]) :Builder[A, PassedArray[A]] =
		new SpecificBuilder(elementType)

		private class SpecificBuilder[A](elemType :Class[A])
			extends ReusableBuilder[A, PassedArray[A]]
		{
			private[this] var elems :Array[A] = _
			private[this] var size = 0
			private[this] var initSize = InitSize
			override def knownSize = size
			protected def knownSize_=(size :Int) :Unit = this.size = size
			protected def elementType = elemType

			def capacity :Int = if (elems == null) 0 else elems.length

			private def newArray(size :Int) :Array[A] =
				if (elementType == null)
					java.lang.reflect.Array.newInstance(classOf[Any], size).asInstanceOf[Array[A]]
				else
					java.lang.reflect.Array.newInstance(elementType, size).asInstanceOf[Array[A]]

			protected final def realloc(newSize :Int) :Unit = {
				val copy = java.lang.reflect.Array.newInstance(elementType, newSize).asInstanceOf[Array[A]]
				Array.copy(elems, 0, copy, 0, this.size)
				elems = copy
			}

			override def sizeHint(size :Int) :Unit =
				if (size > 0)
					if (elementType == null)
						initSize = size
					else if (elems == null)
						elems = newArray(size)
					else if (size > elems.length)
						realloc(size)

			private def ensure(extras :Int) :Unit = {
				if (elems == null)
					elems = newArray(Math.max(extras, initSize))
				else {
					val capacity = elems.length
					val newSize = size + extras
					if (newSize > capacity) {
						val newCapacity = Math.max(capacity * 2, newSize)
						if (initSize > newSize)
							realloc(Math.min(newCapacity, initSize))
						else
							realloc(newCapacity)
					}
				}
			}

			override def addAll(xs :IterableOnce[A]) :this.type = xs match {
				case it :Iterable[A] if it.isEmpty => this
				case it :Iterator[A] if !it.hasNext => this
				case it :Iterable[A] => it.knownSize match {
					case -1 => super.addAll(it)
					case n  =>
						ensure(n)
						assert(it.copyToArray(elems, size) == n)
						size += n
						this
				}
				case _ => super.addAll(xs)
			}

			override def addOne(elem :A) = {
				if (elems == null)
					elems = newArray(initSize)
				else if (size == elems.length)
					elems = Array.copyOf(elems, size * 2)
				elems(size) = elem
				size += 1
				this
			}

			override def clear() :Unit = elems = null

			override def result() =
				if (elems == null)
					Empty
				else {
					val res = new PassedArrayPlus(elems, 0, size, true)
					clear()
					res
				}
		}

	private class OptimisticBuilder[A](private[this] var _elementType :Class[A])
		extends SpecificBuilder[A](_elementType)
	{
		private[this] var optimistic = _elementType == null || elementType.getClass.isPrimitive
		//overriden because we use it much more, so want to use a field and not a method call.
		override def elementType :Class[A] = _elementType
		def elementType_=(tpe :Class[A]) :Unit = _elementType = tpe

		override def addOne(elem :A) :this.type = {
			if (_elementType == null) {
				_elementType = elem.getClass.castParam[A]
				if (!_elementType.isPrimitive)
					_elementType = classOf[Any].castParam[A]
				super.addOne(elem)
			} else if (optimistic)
				if (_elementType isAssignableFrom elem.getClass)
					super.addOne(elem)
				else {
					optimistic = false
					_elementType = classOf[AnyRef].castParam[A]
					realloc(Math.max(capacity, InitSize))
					super.addOne(elem)
				}
			else
				super.addOne(elem)
		}
		override def addAll(xs :IterableOnce[A]) =
			xs match {
				case _ if !optimistic => super.addAll(xs)
				case empty :Iterable[_] if empty.isEmpty => this
				case empty :Iterator[_] if !empty.hasNext => this
				case items :PassedArray[A]      => addKnownType(items.elementType, items)
				case items :ArraySeq[A]         => addKnownType(items.unsafeArray.getClass.getComponentType, items)
				case items :mutable.ArraySeq[A] => addKnownType(items.array.getClass.getComponentType, items)
				case items :ArrayAsSeq[A]     => addKnownType(items.coll.getClass.getComponentType, items)
				case items :Iterable[A]         => addKnownType(items.head.getClass, items)
				case _ =>
					val iter = xs.iterator
					while (iter.hasNext)
						addOne(iter.next())
					this
			}
		private final def addKnownType(elemType :Class[_], xs :IterableOnce[A]) :this.type =
			if (_elementType == null) {
				_elementType = (if (elemType.isPrimitive) elemType else classOf[AnyRef]).castParam[A]
				addOptimistic(xs)
			} else if (!_elementType.isAssignableFrom(elemType)) {
				optimistic = false
				val size = knownSize
				val extras = xs.knownSize
				val newSize =
					if (extras >= 0) Math.max(Math.min(size + extras, size * 2), InitSize)
					else if (size == capacity) Math.max(size * 2, InitSize)
					else size
				realloc(newSize)
				super.addAll(xs)
			} else
				addOptimistic(xs)

		private final def addOptimistic(xs :IterableOnce[A]) :this.type = {
			val rollbackSize = knownSize
			try { super.addAll(xs) } catch {
				case _ :ClassCastException =>
					optimistic = false
					_elementType = classOf[Any].castParam[A]
					knownSize = rollbackSize
					realloc(capacity)
					super.addAll(xs)
			}
		}
	}


	private final val Empty = new PassedArray0

	override def toString = "PassedArray"
}


private object PassedArrayPlus {
	private[collections] val OwnerField = new PassedArrayPlus(Array.emptyIntArray).fieldHandle
}


private object PassedArrayInternals {
	def wrap[E](array :Array[E]) :PassedArray[E] = new PassedArrayPlus(array)

	def wrap[E](array :Array[E], from :Int, until :Int) :PassedArray[E] =
		if (from < 0 | from > array.length | until > array.length)
			throw new IndexOutOfBoundsException("PassedArray([" + array.length + "], " + from + ", " + until + ")")
		else if (until < from)
			throw new IllegalArgumentException("PassedArray([" + array.length + "], " + from + ", " + until + ")")
		else
			new PassedArrayPlus(array, from, until)

	def superElementType(elem1 :Any, elem2 :Any) :Class[_] =
		if (elem1 == null | elem2 == null)
			classOf[Any]
		else
			superElementType(elem1.getClass, elem2.getClass)

	def superElementType(type1 :Class[_], type2 :Class[_]) :Class[_] = {
		val unboxed1 = BoxClass.unbox(type1)
		val unboxed2 = BoxClass.unbox(type2)
		if (unboxed1.isAssignableFrom(unboxed2)) unboxed1
		else if (unboxed2.isAssignableFrom(unboxed1)) unboxed2
		else classOf[AnyRef]
	}

	final val InitSize = 8
	final val SliceReallocationFactor = 4 //slices smaller than 1/4 reallocate the backing array.
	val OwnerField = PassedArrayPlus.OwnerField
//		MethodHandles.lookup().findVarHandle(classOf[PassedArrayPlus[Any]],
//			"isOwner", classOf[Boolean]
//			classOf[PassedArrayPlus[Any]].getName.replace('.', '$') + "$$isOwner", classOf[Boolean]
//		)
}
