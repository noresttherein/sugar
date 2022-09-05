package net.noresttherein.sugar.collection

import java.lang.invoke.MethodHandles

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps}
import scala.collection.{IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.{classTag, ClassTag}

import net.noresttherein.sugar.collection.PassedArray.{InitSize, OwnerField}
import net.noresttherein.sugar.reflect.classExtension

//implicits
import net.noresttherein.sugar.extensions.castTypeParam




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
  * [[net.noresttherein.sugar.collection.PassedArray$ PassedArray]]` `[[net.noresttherein.sugar.collection.PassedArray$.:+ :+]]` e1 `[[net.noresttherein.oldsql.collection.PassedArray!.:+ :+]]` e2`.
  *
  * Updates and other operations typically require `O(n)` time.
  *
  * This provides a very fast sequence implementation excelling at operations such as `fold`, `flatMap`
  * and linear recursive processing, as long as the sequences are not too large, leading to problems
  * with memory allocation.
  *
  * Small sequences may have dedicated implementations, without a backing array.
  * @author Marcin Mościcki
  */
sealed trait PassedArray[@specialized(Specializable.Arg) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, PassedArray, PassedArray[E]] with IterableFactoryDefaults[E, PassedArray]
{
	override def iterableFactory :SeqFactory[PassedArray] = PassedArray
	override def className = "PassedArray"
	private[collection] def elementType :Class[_]

	/** A `PassedArray` being a view on this instance. Equivalent to `slice`, but no new array is allocated
	  * even if the view is a minor fragment of the underlying array (`(until - from)/length` is large).
	  * Additionally, all `slice` operations on the result are also views on this array, without any reallocation.
	  * Adding elements to the view will create a normal, non view instance.
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collection.PassedArray.strict strict]] creates a copy with normal shrinking behaviour.
	  * The result may be a dedicated class if new size is very small.
	  */
	def section(from :Int, until :Int) :PassedArray[E]

	def strict :PassedArray[E] = this

	//specialized
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, PassedArray[E]] =
		PassedArray.newBuilder[E](elementType.asInstanceOf[Class[E]])

}



@SerialVersionUID(1L)
private[collection] final class PassedArray1[@specialized(Specializable.Arg) +E] private[collection]
                                            (override val head :E)
	extends PassedArray[E]
{
	override def elementType = head.getClass
	override def length = 1
	override def apply(i :Int) :E =
		if (i == 0) head else throw new IndexOutOfBoundsException("PassedArray[1](" + i + ")" )

	override def last :E = head
	override def tail :PassedArray[E] = PassedArray.empty
	override def init :PassedArray[E] = PassedArray.empty

	override def section(from :Int, until :Int) :PassedArray[E] = slice(from, until)

	override def slice(from :Int, until :Int) :PassedArray[E] =
		if (until <= from | from > 0 | until <= 0) PassedArray.empty else this

	override def take(n :Int) :PassedArray[E] = if (n > 0) this else PassedArray.empty
	override def drop(n :Int) :PassedArray[E] = if (n > 0) PassedArray.empty else this
	override def takeRight(n :Int) :PassedArray[E] = if (n > 0) this else PassedArray.empty
	override def dropRight(n :Int) :PassedArray[E] = if (n > 0) PassedArray.empty else this


	//extracted for specialization.
	private[this] def seq2(elem1 :E, elem2 :E) = new PassedArray2(elem1, elem2, head.getClass)
	private[this] def large(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)


	override def appended[B >: E](elem :B) :PassedArray[B] =
		try { seq2(head, elem.asInstanceOf[E]) } catch {
			case _ :ClassCastException => new PassedArray2(head, elem, elem.getClass)
		}
	override def prepended[B >: E](elem :B) :PassedArray[B] =
		try { seq2(elem.asInstanceOf[E], head) } catch {
			case _ :ClassCastException => new PassedArray2(elem, head, elem.getClass)
		}


	override def appendedAll[B >: E](suffix :IterableOnce[B]) :PassedArray[B] = suffix match {
		case items :Iterable[B] if items.isEmpty => this
		case items :Iterator[B] if !items.hasNext => this
		case items :PassedArray1[B] => try {
			seq2(head, items.head.asInstanceOf[E])
		} catch {
			case _ :ClassCastException =>
				val elem = items.head
				new PassedArray2[B](head, elem, elem.getClass.asInstanceOf[Class[B]])
		}
		case items :PassedArray[B] => this ++: items
		case items :Iterable[B] => items.knownSize match {
			case 1 => try {
				seq2(head, items.head.asInstanceOf[E])
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new PassedArray2(head, elem, elem.getClass.asInstanceOf[Class[B]])
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
						new PassedArray2[B](head, elem, elem.getClass.asInstanceOf[Class[B]])
				}
				case n => appendedAllLarge(i, n)
			}
	}

	private def appendedAllLarge[B >: E](items :IterableOnce[B], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder += head ++= items.asInstanceOf[Iterable[E]]).result()
			} catch {
				case _ :ClassCastException => (PassedArray.newBuilder[B] += head ++= items).result()
			}
			case  n => try {
				val capacity = Math.min(n + 1, InitSize)
				val array = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[E]]
				items match {
					case it :Iterable[E @unchecked] => it.copyToArray(array, 1)
					case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, 1)
				}
				array(0) = head
				large(array, 0, 1 + n)
			} catch {
				case _ :ClassCastException =>
					val array = new Array[AnyRef](Math.min(1 + n, InitSize)).asInstanceOf[Array[B]]
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
				new PassedArray2[B](items.head, head, items.head.getClass.asInstanceOf[Class[B]])
		}
		case items :PassedArray[B] => items :++ this
		case items :Iterable[B] => items.knownSize match {
			case 1 => try {
				seq2(items.head.asInstanceOf[E], head)
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new PassedArray2(elem, head, elem.getClass.asInstanceOf[Class[B]])
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
						new PassedArray2[B](elem, head, elem.getClass.asInstanceOf[Class[B]])
				}
				case n => prependedAllLarge(i, n)
			}
	}

	private def prependedAllLarge[B >: E](items :IterableOnce[B], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder ++= items.asInstanceOf[Iterable[E]] += head).result()
			} catch {
				case _ :ClassCastException => (PassedArray.newBuilder[B] ++= items += head).result()
			}
			case  n => try {
				val newSize = n + 1
				val capacity = Math.min(newSize, InitSize)
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
					val capacity = Math.min(newSize, InitSize)
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

	private[this] def copy(xs :Array[E], start :Int) :Unit = xs(start) = head
}




@SerialVersionUID(1L)
private[collection] final class PassedArray2[@specialized(Specializable.Arg) +E] private[collection]
                                            (override val head :E, override val last :E, override val elementType :Class[_])
	extends AbstractSeq[E] with PassedArray[E]
{
	override def length = 2
	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => last
		case _ => throw new IndexOutOfBoundsException("PassedArray[2](" + i + ")")
	}

	override def tail :PassedArray[E] = new PassedArray1(last)
	override def init :PassedArray[E] = new PassedArray1(head)

	override def section(from :Int, until :Int) :PassedArray[E] = slice(from, until)

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


	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)

	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val isSubtype = elementType isAssignableFrom elem.getClass
		val tpe = if (isSubtype) elementType else elem.getClass
		val copy = java.lang.reflect.Array.newInstance(tpe, InitSize).asInstanceOf[Array[E]]
		copy(2) = elem.asInstanceOf[E]
		copyToArray(copy, 0, 2)
		if (isSubtype)
			newInstance(copy, 0, 3)
		else
			new PassedArrayPlus[E](copy, 0, 3, true)
	}

	override def prepended[B >: E](elem :B) :PassedArray[B] = {
		val isSubtype = elementType isAssignableFrom elem.getClass
		val tpe = if (isSubtype) elementType else elem.getClass
		val copy = java.lang.reflect.Array.newInstance(tpe, InitSize).asInstanceOf[Array[E]]
		val newOffset = InitSize - 3
		copy(newOffset) = elem.asInstanceOf[E]
		copyToArray(copy, InitSize - 2, 2)
		if (isSubtype)
			newInstance(copy, newOffset, 3)
		else
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
		else if (start >= xs.length)
			0
		else {
			val copied = Math.min(xs.length - start, len)
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




private[collection] sealed trait AbstractPassedArray[@specialized(Specializable.Arg) +E] extends PassedArray[E] {
	override def elementType :Class[_] = elems.getClass.getComponentType
	private[collection] def elems[U >: E] :Array[U]
	private[collection] def startIndex :Int

	override def section(from :Int, until :Int) :PassedArray[E] = {
		val offset = startIndex; val len = length
		if (until <= from | until <= 0 | from >= len)
			PassedArray.empty
		else if (from <= 0 && until >= len)
			this
		else {
			val array   = elems
			val start   = offset + Math.max(from, 0)
			val newSize = Math.min(len, until) - start
			if (newSize == 1)
				new PassedArray1(array(offset + start))
			else if (newSize == 2)
				new PassedArray2(array(offset + start), array(offset + start + 1), elementType)
			else
				new PassedArrayView(array, start, newSize)
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
			val copied = Math.min(length, Math.min(len, Math.max(xs.length - start, 0)))
			Array.copy(elems, startIndex, xs, start, copied)
			copied
		}
}




@SerialVersionUID(1L)
private[collection] final class PassedArrayPlus[@specialized(Specializable.Arg) +E] private[collection]
                                               (array :Array[E], offset :Int, len :Int, owner :Boolean = false)
	extends AbstractSeq[E] with AbstractPassedArray[E]
{
	private[collection] def this(array :Array[E]) = this(array, 0, array.length)

	private[collection] override def elems[U >: E] :Array[U] = array.asInstanceOf[Array[U]]
	private[collection] override def startIndex :Int = offset
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
		else if (from <= 0 && until >= len)
			this
		else {
			val start   = offset + Math.max(from, 0)
			val newSize = Math.min(len, until) - start
			if (newSize == 1)
				new PassedArray1(array(offset + start))
			else if (newSize == 2)
				new PassedArray2(array(offset + start), array(offset + start + 1), elementType)
			else if (newSize < elems.length / 4) {
				val copy = java.lang.reflect.Array.newInstance(elementType, newSize).asInstanceOf[Array[E]]
				Array.copy(array, offset, copy, 0, newSize)
				new PassedArrayPlus(copy)
			} else
				new PassedArrayPlus(array, start, newSize)
		}


	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)

	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore = elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType)
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
		val canStore = elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType)
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
			case it :PassedArray[B] if len == 0 && it.length > array.length || !isOwner => it
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
			case it :PassedArray[B] if len == 0 && it.length > array.length || !isOwner => it
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

}




@SerialVersionUID(1L)
private[collection] final class PassedArrayView[@specialized(Specializable.Arg) +E] private[collection]
                                               (array :Array[E], offset :Int, len :Int)
	extends AbstractSeq[E] with AbstractPassedArray[E]
{
	private[collection] override def elems[U >: E] :Array[U] = array.asInstanceOf[Array[U]]
	private[collection] override def startIndex :Int = offset
	override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			throw new IndexOutOfBoundsException("PassedArray[" + len + "](" + i + ")")
		else
			array(offset + i)

	override def strict :PassedArray[E] = {
		val copy = java.lang.reflect.Array.newInstance(elementType, len).asInstanceOf[Array[E]]
		Array.copy(array, offset, copy, 0, len)
		new PassedArrayPlus(array, 0, len)
	}

	override def slice(from :Int, until :Int) :PassedArray[E] = section(from, until)

	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def passedArray(array :Array[E], offset :Int, len :Int) =
		new PassedArrayPlus(array, offset, len, true)


	override def appended[B >: E](elem :B) :PassedArray[B] = {
		val elemType = elem.getClass
		val canStore = elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType)
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
		val canStore = elementType.isAssignableFrom(elemType) || elemType.isBoxOf(elementType)
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
}






/** $factoryInfo
  * @define Coll `PassedArray`
  * @define coll passed array
  */
object PassedArray extends StrictOptimizedSeqFactory[PassedArray] {
	override def from[A](it :IterableOnce[A]) :PassedArray[A] = it match {
		case elems :PassedArray[A] => elems
		case elems :Iterable[A] if elems.isEmpty => Empty
		case elems :Iterable[A] =>
			new PassedArrayPlus(elems.toArray(classTag[AnyRef].asInstanceOf[ClassTag[A]]))
		case _ =>
			val i = it.iterator
			if (!i.hasNext) Empty
			else new PassedArrayPlus(i.toArray(classTag[AnyRef].asInstanceOf[ClassTag[A]]))
	}

	override def empty[A] :PassedArray[A] = Empty

	@inline def :+[A](elem :A) :PassedArray[A] = single(elem)

	@inline def :++[A](elems :IterableOnce[A]) :PassedArray[A] = from(elems)

	def single[A](elem :A) :PassedArray[A] = new PassedArray1(elem)

	def one[A](elem :A) :PassedArray[A] = new PassedArray1(elem)

	def two[A](first :A, second :A) :PassedArray[A] = new PassedArray2(first, second, classOf[Any])

	def ofCapacity[A](size :Int) :PassedArray[A] = size match {
		case 0 => empty
		case n => new PassedArrayPlus[A](new Array[Any](n).castParam[A], 0, 0, true)
	}

	override def newBuilder[A] :Builder[A, PassedArray[A]] = newBuilder(classOf[AnyRef].asInstanceOf[Class[A]])

	def newBuilder[A](elementType :Class[A]) :Builder[A, PassedArray[A]] =
		new ReusableBuilder[A, PassedArray[A]] {
			private[this] var elems :Array[A] = _
			private[this] var size = 0
			override def knownSize = size

			private def newArray(size :Int) :Array[A] =
				java.lang.reflect.Array.newInstance(elementType, size).asInstanceOf[Array[A]]

			override def sizeHint(size :Int) :Unit =
				if (size > 0)
					if (elems == null)
						elems = newArray(Math.min(size, InitSize))
					else if (size > elems.length) {
						val copy = newArray(Math.min(size, InitSize))
						Array.copy(elems, 0, copy, 0, this.size)
						elems = copy
					}

			private def ensure(extras :Int) :Unit = {
				val capacity = elems.length
				val newSize = size + extras
				if (elems == null)
					elems = newArray(Math.min(extras * 2, InitSize))
				else if (newSize < capacity) {
					val newCapacity =
						if (capacity * 2 > newSize) capacity * 2
						else newSize
					val copy = newArray(newCapacity)
					Array.copy(elems, 0, copy, 0, size)
					elems = copy
				}
			}

			override def addAll(xs :IterableOnce[A]) :this.type = xs match {
				case it :Iterable[A] if it.isEmpty => this
				case it :Iterator[A] if !it.hasNext => this
				case it :Iterable[A] => it.knownSize match {
					case -1 => super.addAll(it)
					case n  =>
						ensure(n)
						it.copyToArray(elems, size)
						size += n
						this
				}
				case _ => super.addAll(xs)
			}

			override def addOne(elem :A) = {
				if (elems == null)
					elems = newArray(InitSize)
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


	private object Empty extends AbstractSeq[Nothing] with PassedArray[Nothing] {
		override def elementType = classOf[AnyRef]
		override def length = 0
		override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException(i)

		override def section(from :Int, until :Int) :PassedArray[Nothing] = this
		override def slice(from :Int, until :Int) :PassedArray[Nothing] = this
		override def take(n :Int) :PassedArray[Nothing] = this
		override def drop(n :Int) :PassedArray[Nothing] = this
		override def takeRight(n :Int) :PassedArray[Nothing] = this
		override def dropRight(n :Int) :PassedArray[Nothing] = this

		override def appended[B >: Nothing](elem :B) :PassedArray[B] = new PassedArray1(elem)
		override def prepended[B >: Nothing](elem :B) :PassedArray[B] = new PassedArray1(elem)
		override def appendedAll[B >: Nothing](suffix :IterableOnce[B]) :PassedArray[B] = from(suffix)
		override def prependedAll[B >: Nothing](prefix :IterableOnce[B]) :PassedArray[B] = from(prefix)

		override def copyToArray[B >: Nothing](xs :Array[B], start :Int, len :Int) :Int = 0
	}

	private[collection] final val InitSize = 8
	private[collection] val OwnerField =
		MethodHandles.lookup().findVarHandle(classOf[PassedArrayPlus[Any]], "isOwner", classOf[Boolean])

}
