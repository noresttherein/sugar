package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.lang.invoke.MethodHandles

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterable, Factory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedSeqFactory, View, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JIterator, JLong, JShort}
import net.noresttherein.sugar.arrays.{ArrayFactory, ArrayLike, ErasedArray, IArray, IRefArray}
import net.noresttherein.sugar.collections.CompanionFactory.sourceCollectionFactory
import net.noresttherein.sugar.collections.HasFastSlice.preferDropOverIterator
import net.noresttherein.sugar.collections.RelayArrayInternals.{AcceptableBuilderFillRatio, InitSize, OwnerField, SliceReallocationFactor, superElementType}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.concurrent.releaseFence
import net.noresttherein.sugar.extensions.{IsIterableOnceExtension, IterableExtension}
import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.reflect.{PrimitiveClass, Unboxed}
import net.noresttherein.sugar.vars.Maybe.Yes

//implicits
import net.noresttherein.sugar.extensions.{ArrayCompanionExtension, classNameMethods, castTypeParamMethods, ClassExtension, IteratorCompanionExtension, castingMethods}




//Consider: a supertype for RelayArrayView, ReversedSeq, SeqSlice, etc. AdapterSeq, Subcollection?
//todo: introduce a supertype, and make RelayArrayView a subtype of it, apart from other subclasses.
//todo: RelaySeq; RelayArray1=>RelaySeq1, RelayArray2=>RelaySeq2, maybe RelayArraySeq which will shrink to RelaySeq?
//todo: RelayMatrix: a two dimensional array when size exceeds a threshold.

/** An array-backed immutable sequence with amortized `O(1)` random access, `slice` and first `append/prepend`.
  * It combines the idea behind the classic growing and shrinking mutable buffer, a dope vector,
  * and passed ownership pattern. It is a view of a section of an array, potentially uninitialized past its bounds.
  * All slicing operations which take above a certain percentage of the whole array are implemented in `O(1)`
  * by passing the same array with new bounds; only when usage drops below a threshold reallocation takes place.
  * Additionally, each instance has a mutable ownership flag, signifying that:
  *   - the array past that instance's bounds is unused by any other instance, and
  *   - it is the only object in existence backed by the same array with that flag set.
  *
  * Whenever new elements are appended/prepended, the instance is the owner of the array, and required space exists
  * before/after its elements, the new elements will be copied to the same array, which will be shared
  * with the created sequence. The ownership flag in that case is passed on to the new instance.
  * {{{
  *     val s1 = RelayArray(1, 2, 3) //backed by a1 = Array[Int](1, 2, 3)
  *     val s2 = s1 :+ 4             //backed by a2 = Array[Int](1, 2, 3, 4, _, _)
  *     val s3 = s1 :++ Seq(5, 6)    //backed by the same a2, now equal Array[Int](1, 2, 3, 4, 5, 6)
  * }}}
  * When creating an instance for a given array, the underlying array will be of the same type. The array type
  * can be also specified explicitly
  * for a [[net.noresttherein.sugar.collections.RelayArray.newBuilder[E](elementType* builder]]. Otherwise,
  * the array type will be inferred optimistically from stored elements: if all elements in the sequence
  * are box objects for the same value type, the elements will be stored in a value array. Otherwise, a universal
  * `Array[Any]` is used. Moreover, when adding new elements to a sequence, the new sequence will initially assume
  * they will be of a compatible type (the same value type if the sequence is backed by a value array),
  * and growing an array will first create an array of the same type. However, if an element is encountered
  * which would throw [[ArrayStoreException]], the underlying array is replaced with an `Array[Any]`.
  * {{{
  *     val a1 = RelayArray(1, 2, 3) :+ 4   //backed by an Array[Int]
  *     val a2 = RelayArray(1, 2, 3) :+ "4" //backed by an Array[Any]
  * }}}
  *
  * Updates and other operations typically require `O(n)` time.
  *
  * This makes it a very fast sequence implementation excelling both at random access, and at sequential read
  * (including operations such as `fold`, `flatMap`, etc). At the same time, because [[collection.IterableOps.tail tail]]
  * and slicing are still a cheap `O(1)` operation, it is well suited for recursive processing,
  * which makes it a sensible alternative to [[collection.immutable.List List]], as long as the sequences
  * are not very long, which makes allocation of contiguous memory expensive. However, as a slice is typically
  * backed by the same array as the original sequence, it retains strong references to all elements of the latter,
  * including those ''which are not its own elements''. For this reason, it is recommended to slice only sequences
  * of value types or other small objects.
  *
  * Very short sequences have dedicated implementations, without a backing array.
  * @define Coll `RelayArray`
  * @define coll relay array
  * @author Marcin Mościcki
  */ //todo: BooleanRelayArray implementation
sealed trait RelayArray[@specialized(ElemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, RelayArray, RelayArray[E]]
	   with StrictOptimizedSeqOps[E, RelayArray, RelayArray[E]] with IterableFactoryDefaults[E, RelayArray]
	   with SugaredIterable[E] with SugaredSeqOps[E, RelayArray, RelayArray[E]] with Serializable
{

	override def iterableFactory :SeqFactory[RelayArray] = RelayArray
	protected override def className = "RelayArray"
	def elementType :Class[_]

	/** A `PassedArray` being a view on this instance. Calling `slice` on the result will never allocate a new array
	  * to copy the contents, even if the view is a minor fragment of the underlying array
	  * (`(until - from)/length` is large). Adding elements to the view, mapping or any other operations
	  * which do not return a continuous subsequence of this sequence will create a normal, non view instance.
	  * This makes `tail` and other slicing operations very fast, and the implementation particularly suited towards
	  * list-like processing. Due to the possibility of inducing a memory leak, this implementation should be used
	  * only by code which is certain not to expose any of the slices back to the application.
	  *
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collections.RelayArray.safe safe]] creates a copy with normal shrinking behaviour.
	  * The result may be a dedicated class if new size is very small.
	  * @return [[net.noresttherein.sugar.collections.RelayArray.window(from:Int* window]]`(0, length)`.
	  */ //consider: renaming to subseq or range
	def window :RelayArray[E] = window(0, length)

	/** A `PassedArray` being a view on this instance. Equivalent to `slice`, but no new array is allocated
	  * even if the view is a minor fragment of the underlying array (`(until - from)/length` is large).
	  * Additionally, all `slice` operations on the result are also views on this array, without any reallocation.
	  * Adding elements to the view will create a normal, non view instance. This makes `tail` and other
	  * slicing operations very fast, and the implementation particularly suited towards list-like processing.
	  * Due to the possibility of inducing a memory leak, this implementation should be used only by code which
	  * is certain not to expose any of the slices back to the application.
	  *
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collections.RelayArray.safe safe]] creates a copy with normal shrinking behaviour.
	  * The result may be a dedicated class if new size is very small.
	  */
	def window(from :Int, until :Int) :RelayArray[E]

	/** A version of this sequence guarded against memory leaks caused by slicing.
	  * The default implementation is already safe (returns `this`); this method is used in conjunction
	  * with [[net.noresttherein.sugar.collections.RelayArray.window window]], which creates
	  * a view over the underlying array which never copies elements while slicing, regardless of the ratio
	  * between the slice length and the underlying array length.
	  */ //consider: renaming to shrinking, frugal
	def safe :RelayArray[E] = this

	/** Reallocates (if needed) the data to a new array of an appropriate size to free the larger array
	  * for garbage collection (unless used by another instance).
	  */ //consider: renaming to pack
	def compact :RelayArray[E] = this

	//specialized
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, RelayArray[E]] =
		RelayArray.newBuilder[E](elementType.asInstanceOf[Class[E]])

	//todo: remove these once the bug is fixed in SeqOps
	override def startsWith[B >: E](that :IterableOnce[B], offset :Int) :Boolean =
		offset >= 0 && offset <= length && super.startsWith(that, offset)

	override def indexOfSlice[B >: E](that :collection.Seq[B], from :Int) :Int =
		if (from > length) -1
		else super.indexOfSlice(that, math.max(0, from))

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		cyclicCopyRangeToArray(xs, start, 0, len)

	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		copyRangeToArray(xs, start, 0, len)

	protected override def applyPreferredMaxLength :Int = RelayArrayInternals.applyPreferredMaxLength
}



/** An empty `RelayArray`. */
@SerialVersionUID(Ver)
private class RelayArray0
	extends AbstractSeq[Nothing] with RelayArray[Nothing] with EmptyIndexedSeqOps.Generic[RelayArray]
{
	protected override def one[T](elem :T) :RelayArray[T] = new RelayArray1(elem)
	override def elementType = classOf[AnyRef]
	override def window(from :Int, until :Int) :this.type = this
}



/** A [[net.noresttherein.sugar.collections.RelayArray RelayArray]] with a single element. */
@SerialVersionUID(Ver)
private class RelayArray1[@specialized(ElemTypes) +E] private[collections](override val head :E)
	extends AbstractSeq[E] with RelayArray[E] with SingletonIndexedSeqOps.Generic[E, RelayArray]
{
	protected override def one[T](elem :T) :RelayArray[T] = RelayArray.one(elem)
	protected override def two[T](first :T, second :T) :RelayArray[T] = RelayArray.two(first, second)

	override def elementType = Unboxed(head.getClass)

	override def window(from :Int, until :Int) :RelayArray[E] = slice(from, until)

	//extracted for specialization.
	private[this] def seq1(elem :E) = new RelayArray1(elem)
	private[this] def seq2(elem1 :E, elem2 :E) = new RelayArray2(elem1, elem2)

	private[this] def large(array :Array[E], offset :Int, len :Int) =
		new RelayArrayPlus(array, offset, len, true)

	override def updated[U >: E](index :Int, elem :U) :RelayArray[U] =
		if (index != 0) outOfBounds_!(index, this)
		else if (head.getClass isAssignableFrom elem.getClass) seq1(elem.asInstanceOf[E])
		else one(elem)

	override def inserted[U >: E](index :Int, elem :U) :RelayArray[U] =
		if (index < 0 | index > 1) outOfBounds_!(index, errorString(this) + ".inserted")
		else if (head.getClass isAssignableFrom elem.getClass)
			if (index == 0) seq2(elem.asInstanceOf[E], head)
			else seq2(head, elem.asInstanceOf[E])
		else
			if (index == 0) two(elem, head) else two(head, elem)

	override def appended[U >: E](elem :U) :RelayArray[U] =
		try { seq2(head, elem.asInstanceOf[E]) } catch {
			case _ :ClassCastException => new RelayArray2(head, elem)
		}
	override def prepended[U >: E](elem :U) :RelayArray[U] =
		try { seq2(elem.asInstanceOf[E], head) } catch {
			case _ :ClassCastException => new RelayArray2(elem, head)
		}

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] = suffix match {
		case items :Iterable[U] if items.isEmpty => this
		case items :Iterator[U] if !items.hasNext => this
		case items :RelayArray1[U] => try {
			seq2(head, items.head.asInstanceOf[E])
		} catch {
			case _ :ClassCastException =>
				val elem = items.head
				new RelayArray2[U](head, elem)
		}
		case items :RelayArray[U] => this ++: items
		case items :Iterable[U] => items.knownSize match {
			case 1 => try {
				seq2(head, items.head.asInstanceOf[E])
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new RelayArray2(head, elem)
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
						new RelayArray2[U](head, elem)
				}
				case n => appendedAllLarge(i, n)
			}
	}

	private def appendedAllLarge[U >: E](items :IterableOnce[U], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder += head ++= items.asInstanceOf[Iterable[E]]).result()
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					(RelayArray.genericBuilder[U] += head ++= items).result()
			}
			case  n => try {
				val capacity = math.max(n + 1, InitSize)
				val array = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
				items match {
					case it :Iterable[E @unchecked] => it.copyToArray(array, 1)
					case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, 1)
				}
				array(0) = head
				large(array, 0, 1 + n)
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					val array = new Array[AnyRef](math.max(1 + n, InitSize)).asInstanceOf[Array[U]]
					items match {
						case it :Iterable[U] => it.copyToArray(array, 1)
						case _ => items.iterator.copyToArray(array, 1)
					}
					array(0) = head
					new RelayArrayPlus(array, 0, 1 + n, true)
			}
		}


	override def prependedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] = suffix match {
		case items :Iterable[U] if items.isEmpty => this
		case items :Iterator[U] if !items.hasNext => this
		case items :RelayArray1[U] => try {
			seq2(items.head.asInstanceOf[E], head)
		} catch {
			case _ :ClassCastException =>
				new RelayArray2[U](items.head, head)
		}
		case items :RelayArray[U] => items :++ this
		case items :Iterable[U] => items.knownSize match {
			case 1 => try {
				seq2(items.head.asInstanceOf[E], head)
			} catch {
				case _ :ClassCastException =>
					val elem = items.head
					new RelayArray2(elem, head)
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
						new RelayArray2[U](elem, head)
				}
				case n => prependedAllLarge(i, n)
			}
	}

	private def prependedAllLarge[U >: E](items :IterableOnce[U], knownSize :Int) =
		knownSize match {
			case -1 => try {
				(newSpecificBuilder ++= items.asInstanceOf[Iterable[E]] += head).result()
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					(RelayArray.genericBuilder[U] ++= items += head).result()
			}
			case  n => try {
				val newSize = n + 1
				val capacity = math.max(newSize, InitSize)
				val array = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
				items match {
					case it :Iterable[E @unchecked] => it.copyToArray(array, capacity - newSize)
					case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, capacity - newSize)
				}
				array(capacity - 1) = head
				large(array, capacity - newSize, newSize)
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					val newSize = n + 1
					val capacity = math.max(newSize, InitSize)
					val array = new Array[AnyRef](capacity).asInstanceOf[Array[U]]
					items match {
						case it :Iterable[U] => it.copyToArray(array, capacity - newSize)
						case _ => items.iterator.copyToArray(array, capacity - newSize)
					}
					array(capacity - 1) = head
					new RelayArrayPlus(array, capacity - newSize, newSize, true)
			}
		}


	override def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
		if (start >= xs.length | len <= 0 | from > 0)
			0
		else if (start < 0)
			outOfBounds_!(start)
		else if (start >= xs.length)
			0
		else try {
			copy(xs.asInstanceOf[Array[E]], start); 1
		} catch {
			case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException => xs(start) = head; 1
		}

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		copyRangeToArray(xs, start % xs.length, from, len)

	private def copy(xs :Array[E @uncheckedVariance], start :Int) :Unit = xs(start) = head
}




/** A [[net.noresttherein.sugar.collections.RelayArray RelayArray]] of two elements. */
@SerialVersionUID(Ver)
private final class RelayArray2[@specialized(Specializable.Arg) +E] private[collections]
	                            (override val head :E, override val last :E, override val elementType :Class[_])
	extends AbstractSeq[E] with RelayArray[E] with SeqSlicingOps[E, RelayArray, RelayArray[E]]
{
	def this(head :E, last :E) = this(head, last, superElementType(head, last))
	override def iterator = Iterator.two(head, last)
	override def reverseIterator = Iterator.two(last, head)

	override def javaIterator[I <: JavaIterator[_]](implicit shape :JavaIteratorShape[E, I]) :I =
		JavaIterator.two(head, last)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		Stepper2(head, last)

	override def length = 2

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => last
		case _ => outOfBounds_!("RelayArray|2|(" + i + ")")
	}

	override def window(from :Int, until :Int) :RelayArray[E] = slice(from, until)

	protected override def clippedSlice(from :Int, until :Int) :RelayArray[E] = from match {
		case 0 => new RelayArray1(head) //because 0 < until - from < 2
		case _ => new RelayArray1(last)
	}

	override def filter(pred :E => Boolean) :RelayArray[E] =
		if (pred(head))
			if (pred(last)) this else new RelayArray1(head)
		else
			if (pred(last)) new RelayArray1(last) else RelayArray.empty

	override def filterNot(pred :E => Boolean) :RelayArray[E] =
		if (pred(head))
			if (pred(last)) RelayArray.empty else new RelayArray1(last)
		else
			if (pred(last)) new RelayArray1(head) else this

	override def partition(p :E => Boolean) :(RelayArray[E], RelayArray[E]) =
		if (p(head))
			if (p(last)) (this, RelayArray.empty) else (new RelayArray1(head), new RelayArray1(last))
		else
			if (p(last)) (new RelayArray1(last), new RelayArray1(head)) else (RelayArray.empty, this)


	override def updated[U >: E](index :Int, elem :U) :RelayArray[U] = index match {
		//equals may be expensive
		case 0 if (elementType.isPrimitive || elementType.isBox) && elem == head => this
		case 0 => RelayArray.two(elem, last)
		case 1 if (elementType.isPrimitive || elementType.isBox) && elem == last => this
		case 1 => RelayArray.two(head, elem)
		case _ => outOfBounds_!(toString + ".updated(" + index + ", " + elem + ")")
	}

	override def inserted[U >: E](index :Int, elem :U) :RelayArray[U] = {
		val copy = newArray[U](elem.getClass, InitSize)
		copy(index) = elem
		index match {
			case 0 => copyRangeToArray(copy, 1, 0, 2)
			case 1 => copyRangeToArray(copy, 0, 0, 1); copyRangeToArray(copy, 2, 1, 1)
			case 2 => copyRangeToArray(copy, 0, 0, 2)
			case _ => outOfBounds_!(index, errorString(this) + ".inserted")
		}
		RelayArrayInternals.make(copy, 0, 3)
	}

	override def appended[U >: E](elem :U) :RelayArray[U] = {
		val copy = newArray(elem.getClass, InitSize)
		copy(2) = elem.asInstanceOf[E]
		copyToArray(copy, 0, 2)
		RelayArrayInternals.make(copy, 0, 3)
	}

	override def prepended[U >: E](elem :U) :RelayArray[U] = {
		val copy = newArray(elem.getClass, InitSize)
		val newOffset = InitSize - 3
		copy(newOffset) = elem.asInstanceOf[E]
		copyToArray(copy, InitSize - 2, 2)
		RelayArrayInternals.make(copy, newOffset, 3)
	}

	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = elems match {
		case _ if index < 0 | index > 2               => outOfBounds_!(index, errorString(this) + ".updatedAll")
		case _ if elems.knownSize == 0                => this
		case _  :View[U]                              => updatedAll(index, view.iterator)
		case us :Iterable[U] if us.isEmpty            => this
		case it :Iterator[U] if !it.hasNext           => this
		case us :Iterable[U] if us.sizeIs > 2 - index =>
			outOfBounds_!(
				errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + "): size > " + (2 - index)
			)
		case us :Iterable[U] => index match {
			case 0 if us.size == 1 => RelayArray.two(us.head, last)
			case 0                 => RelayArray.two(us.head, us.last)
			case 1                 => RelayArray.two(head, us.head)
		}
		case _ =>
			val it = elems.iterator
			val res = index match {
				case 0 =>
					val first = it.next()
					val second = if (it.hasNext) it.next() else last
					RelayArray.two(first, second)
				case 1 =>
					RelayArray.two(head, it.next())
			}
			if (it.hasNext)
				outOfBounds_!(
					errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + "): size > " + (2 - index)
				)
			res
	}

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		val elemsSize = elems.knownSize
		elems match {
			case _ if index >= 2 | elemsSize == 0 || elemsSize > 0 && index <= 0 && index + elemsSize <= 0 => this
			case _  :View[U]                    => overwritten(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case it :Iterable[U] => index match {
				case 0 if it.sizeIs >= 2             => RelayArray.two(it.head, it.second)
				case 0                               => RelayArray.two(it.head, last)
				case 1                               => RelayArray.two(head, it.head)
				case _ if preferDropOverIterator(it) => overwritten(0, it.drop(-index))
				case _                               => overwritten(0, it.iterator.drop(-index))
			}
			case _ =>
				val it = elems.iterator
				index match {
					case _ if !it.hasNext => this
					case 1                => RelayArray.two(head, it.next())
					case 0                =>
						val first = it.next()
						RelayArray.two(first, if (it.hasNext) it.next() else last)
					case _ =>
						overwritten(0, it.drop(-index))
				}
		}
	}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		val size = elems.knownSize
		elems match {
			case _ if index >= 2 | size == 0 | size > 0 & index <= 0 & size <= -index => this
			case _  :View[U]                    => insertedAll(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case ar :RelayArray[U] if ar.elementType isConvertibleTo elementType =>
				val res = newArray(size + 2)
				ar.copyToArray(res.asInstanceOf[Array[U]], index, Int.MaxValue)
				index match {
					case 0 => copyToArray(res, size, 2)
					case 1 => copyToArray(res, 0, 1); copyRangeToArray(res, size + 1, 1, 1)
					case 2 => copyToArray(res, 0, 2)
				}
				newInstance(res, 0, size + 2)
			case _ if size > 0 =>
				val res = RelayArray.newBuilder[U]
				index match {
					case 0 => res ++= elems += head += last
					case 1 => res += head ++= elems += last
					case 2 => res += head += last ++= elems
				}
				res.result()
		}
	}

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] =
		suffix match {
			case it :Iterable[U] if it.isEmpty => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] =>
				if (it.elementType isConvertibleTo elementType)
					appendedAll(it.asInstanceOf[RelayArray[E]])
				else {
					val newSize = it.length + 2
					val capacity = math.max(newSize, InitSize)
					val copy = new Array[Any](capacity).asInstanceOf[Array[U]]
					copy(0) = head
					copy(1) = last
					it.copyToArray(copy, 2)
					new RelayArrayPlus(copy, 0, newSize, true)
				}
			case it :Iterable[U] => it.knownSize match {
				case -1 => super.appendedAll(it)
				case  n => try {
					val capacity = math.max(n + 2, InitSize)
					val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
					write(copy, 0, 0, 2)
					it.copyToArray(copy.asInstanceOf[Array[U]], 2)
					newInstance(copy, 0, n + 2)
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						val capacity = math.max(n + 2, InitSize)
						val copy = new Array[AnyRef](capacity).asInstanceOf[Array[U]]
						copy(0) = head
						copy(1) = last
						it.copyToArray(copy, 2, n + 2)
						new RelayArrayPlus(copy, 0, n + 2, true)
				}
			}
			case _ => super.appendedAll(suffix)
		}

	private[this] def appendedAll(suffix :RelayArray[E]) =
		if (suffix.isEmpty)
			this
		else {
			val newSize = suffix.length + 2
			val capacity = math.max(newSize, InitSize)
			val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
			copy(0) = head
			copy(1) = last
			suffix.copyToArray(copy, 2)
			new RelayArrayPlus(copy, 0, newSize, true)
		}


	override def prependedAll[U >: E](prefix :IterableOnce[U]) :RelayArray[U] =
		prefix match {
			case it :Iterable[U] if it.isEmpty => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] =>
				if (it.elementType isConvertibleTo elementType)
					prependedAll(it.asInstanceOf[RelayArray[E]])
				else {
					val newSize = it.length + 2
					val capacity = math.max(newSize, InitSize)
//					val copy = java.lang.reflect.Array.newInstance(elementType, capacity).asInstanceOf[Array[B]]
					val copy = new Array[Any](capacity).asInstanceOf[Array[U]]
					copy(capacity - 2) = head
					copy(capacity - 1) = last
					it.copyToArray(copy, capacity - newSize)
					new RelayArrayPlus(copy, capacity - newSize, newSize, true)
				}
			case it :Iterable[U] => it.knownSize match {
				case -1 => super.prependedAll(it)
				case  n => try {
					val capacity = math.max(n + 2, InitSize)
					val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
					write(copy, 0, capacity - 2, 2)
					val newOffset = capacity - 2 - n
					it.copyToArray(copy.asInstanceOf[Array[U]], newOffset, n)
					newInstance(copy, newOffset, n + 2)
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						val capacity = math.max(n + 2, InitSize)
						val copy = new Array[AnyRef](capacity).asInstanceOf[Array[U]]
						copy(capacity - 2) = head
						copy(capacity - 1) = last
						val newOffset = capacity - 2 - n
						it.copyToArray(copy, newOffset, n)
						new RelayArrayPlus[U](copy, newOffset, n + 2, true)
				}
			}
			case _ => super.prependedAll(prefix)
		}

	private[this] def prependedAll(prefix :RelayArray[E]) =
		if (prefix.isEmpty)
			this
		else {
			val newSize = prefix.length + 2
			val capacity = math.max(newSize, InitSize)
			val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
			copy(newSize - 2) = head
			copy(newSize - 1) = last
			prefix.copyToArray(copy, 0)
			new RelayArrayPlus(copy, 0, newSize, true)
		}

	private[this] def newArray(length :Int) :Array[E] = ArrayFactory.ofDim(elementType.castParam[E], length)

	private[this] def newArray[U >: E](tpe :Class[_], length :Int) :Array[U] =
		if (Unboxed(elementType) isAssignableFrom Unboxed(tpe)) newArray(length).asInstanceOf[Array[U]]
		else ArrayFactory.ofDim(tpe.castParam[U], length)

	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new RelayArrayPlus(array, offset, len, true)


	override def copyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from >= 2 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(start)
		else {
			val from0  = math.min(math.max(from, 0), 2)
			val copied = math.min(2 - from0, math.min(xs.length - start, len))
			def copy() :Unit = from match {
				case 1 => xs(start) = last
				case 0 =>
					xs(start) = head
					if (copied > 1)
						xs(start + 1) = last
				case _ =>
			}
			if (copied > 0)
				try {
					val xsType = xs.getClass.getComponentType
					if (xsType.isAssignableFrom(elementType))
						write(xs.asInstanceOf[Array[E]], from, start, copied)
					else
						copy()
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						copy()
				}
			copied
		}

	override def cyclicCopyRangeToArray[A >: E](xs :Array[A], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from >= 2)
			0
		else if (start < 0)
			outOfBounds_!(start)
		else {
			val length = xs.length
			val start0 = start % length
			val from0  = math.min(math.max(from, 0), 2)
			val copied = math.min(2 - from0, math.min(length, len))
			def copy() :Unit = from match {
				case 1 => xs(start) = last
				case 0 =>
					xs(start0) = head
					if (copied > 1)
						xs((start0 + 1) % length) = last
				case _ =>
			}
			if (copied > 0)
				try {
					val xsType = xs.getClass.getComponentType
					if (xsType.isAssignableFrom(elementType))
						write(xs.asInstanceOf[Array[E]], from, start, copied)
					else
						copy()
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						copy()
				}
			copied
		}

	private[this] def write(xs :Array[E], from :Int, start :Int, len :Int) :Unit =
		if (len > 1) {
			xs(start) = head
			xs(start + 1) = last
		} else if (len == 1)
			xs(start) = if (from == 0) head else last

}



/** Most of the implementation of an actual array backed [[net.noresttherein.sugar.collections.RelayArray RelayArray]].
  * Base trait for [[net.noresttherein.sugar.collections.RelayArrayPlus RelayArrayPlus]]
  * and [[net.noresttherein.sugar.collections.RelayArrayView RelayArrayView]].
  */ //todo: rename to SubRelayArray or RelayArraySlice; problem with 'Slice' is slice method from Iterable
private sealed trait ProperRelayArray[@specialized(ElemTypes) +E]
	extends RelayArray[E] with ArraySliceSeqOps[E, RelayArray, RelayArray[E]]
{
	override def elementType :Class[_] = unsafeArray.getClass.getComponentType
	override def isImmutable :Boolean = true

	private[collections] def dumpString :String =
		mkString(s"${this.localClassName}|$startIndex-${startIndex + length}/${ unsafeArray.length}|(", ", ", ")")

	override def updated[B >: E](index :Int, elem :B) :RelayArray[B] = {
		val len = length
		if (index < 0 | index >= len)
			outOfBounds_!(index.toString + " out of " + len)
		val array = unsafeArray.asInstanceOf[Array[B]]
		val offset = startIndex
		val elemType = elementType
		if ((elemType.isPrimitive || elemType.isBox) && array(offset + index) == elem)
			this
		else {
			val res =
				if (elem.getClass isConvertibleTo elemType)
					RelayArray.newBuilder(elemType.castParam[B])
				else
					RelayArray.newBuilder[B]
			res sizeHint len + 1
			if (index > 0)
				res ++= new RelayArrayPlus(array, offset, index)
			res += elem
			if (index < len - 1)
				res ++= new RelayArrayPlus(array, offset + index + 1, len - index - 1)
			res.result()
		}
	}

	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		val size   = elems.knownSize
		val length = this.length
		def overwriteWith(elemType :Class[_]) :RelayArray[U] = {
			val res =
				if (elemType.isConvertibleTo(elementType))
					ArrayFactory.copyOf(array, length).asInstanceOf[Array[U]]
				else
					ArrayFactory.copyAs[U](array, classOf[Any], length)
			elems.toBasicOps.copyToArray(res, index, Int.MaxValue)
			RelayArrayInternals.wrap(res)
		}
		elems match {
			case _ if index < 0 | index > length | size >= length - index =>
				outOfBounds_!(index, errorString(this) + ".updatedAll")
			case _ if size == 0                               => this
			case _  :View[U]                                  => updatedAll(index, view.iterator)
			case it :Iterable[U] if it.isEmpty                => this
			case it :Iterator[U] if !it.hasNext               => this
			case _ if size > 0 && elementType == classOf[Any] => overwriteWith(classOf[Any])
			case ar :RelayArray[U]                            => overwriteWith(ar.elementType)
			case ArrayLike.Wrapped.Slice(array, _, _)         => overwriteWith(array.getClass.getComponentType)
			case _ =>
				val it  = elems.iterator
				if (!it.hasNext)
					this
				else {
					var res  = ArrayFactory.copyOf(array, length).asInstanceOf[Array[U]]
					var i    = index
					var next = it.next()
					try {
						res(i) = next
						i += 1
						while (it.hasNext) {
							next   = it.next()
							res(i) = next //Will throw IndexOutOfBoundsException for us.
							i += 1
						}
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException =>
							res = ArrayFactory.copyAs[U](res, classOf[Any], length)
							res(i) = next
							while (it.hasNext) {
								i += 1
								res(i) = it.next()
							}
					}
					RelayArrayInternals.wrap(res)
				}
		}
	}

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		val size   = elems.knownSize
		val length = this.length
		val offset = math.max(index, 0)
		val drop   = math.max(-index, 0)
		def overwriteWith(elemType :Class[_]) :RelayArray[U] = {
			val res =
				if (elemType.isConvertibleTo(elementType))
					ArrayFactory.copyOf(array, length).asInstanceOf[Array[U]]
				else
					ArrayFactory.copyAs[U](array, classOf[Any], length)
			elems.copyRangeToArray(res, offset, drop, Int.MaxValue)
			RelayArrayInternals.wrap(res)
		}
		elems match {
			case _ if index >= length | size == 0 | size > 0 & size <= drop => this
			case _  :View[U]                          => overwritten(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty        => this
			case it :Iterator[U] if !it.hasNext       => this
			case _ if elementType == classOf[Any]     => overwriteWith(classOf[Any])
			case ar :RelayArray[U]                    => overwriteWith(ar.elementType)
			case ArrayLike.Wrapped.Slice(array, _, _) => overwriteWith(array.getClass.getComponentType)
			case _ =>
				val it = elems.iterator.drop(drop)
				if (!it.hasNext)
					this
				else {
					var next = it.next()
					var i = offset
					var res = ArrayFactory.copyOf(array, length).asInstanceOf[Array[U]]
					try {
						res(i) = next
						i += 1
						while (i < length && it.hasNext) {
							next   = it.next()
							res(i) = next
							i += 1
						}
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException =>
							res = ArrayFactory.copyAs[U](res, classOf[Any], length)
							res(i) = next
							it.copyToArray(res, i + 1, Int.MaxValue)
					}
					RelayArrayInternals.wrap(res)
				}
		}
	}

	override def inserted[U >: E](index :Int, elem :U) :RelayArray[U] =
		if (index == 0) prepended(elem)
		else if (index == length) appended(elem)
		else if (index < 0 || index > length) outOfBounds_!(index, errorString(this) + ".inserted")
		else {
			val length = this.length
			val res = newArray(elem.getClass.castParam[U], length + 1)
			copyRangeToArray(res, 0, 0, index)
			res(index) = elem
			copyRangeToArray(res, index + 1, index, length - index)
			RelayArrayInternals.wrap(res)
		}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		def insertAs(elemType :Class[_], elemsSize :Int) :RelayArray[U] = {
			val res =
				if (elemType.isConvertibleTo(elementType))
					newArray(elementType.castParam[U], length + elemsSize)
				else
					newArray(classOf[Any].castParam[U], length + elemsSize)
			copyRangeToArray(res, 0, 0, index)
			elems.toBasicOps.copyToArray(res, index, Int.MaxValue)
			copyRangeToArray(res, index + elemsSize, index, Int.MaxValue)
			RelayArrayInternals.wrap(res)
		}
		if (index == 0) prependedAll(elems)
		else if (index == length) appendedAll(elems)
		else if (index < 0 || index > length) outOfBounds_!(index, errorString(this) + ".insertedAll")
		else if (elems.knownSize == 0) this
		else elems match {
			case other :RelayArray[U]                        => insertAs(other.elementType, other.length)
			case ArrayLike.Wrapped.Slice(array, from, until) => insertAs(array.getClass.getComponentType, until - from)
			case _ =>
				val res = RelayArray.newBuilder[U]
				res.sizeHint(elems, length)
				res ++= window(0, index) ++= elems ++= window(index, length)
				res.result()
		}
	}


	override def window(from :Int, until :Int) :RelayArray[E] = {
		val offset = startIndex; val len = length
		if (until <= from | until <= 0 | from >= len)
			RelayArray.empty
		else if (from <= 0 && until >= len)
			this
		else {
			val array   = unsafeArray.asInstanceOf[Array[E]]
			val start   = math.max(from, 0)
			val newSize = math.min(len, until) - start
			if (newSize == 1)
				new RelayArray1(array(offset + start))
			else if (newSize == 2)
				new RelayArray2(array(offset + start), array(offset + start + 1), elementType)
			else
				new RelayArrayView(array, offset + start, newSize)
		}
	}

	override def compact :RelayArray[E] =
		if (unsafeArray.length == length)
			this
		else {
			val copy = unsafeArray.asInstanceOf[Array[E]].slice(startIndex, startIndex + length)
			new RelayArrayPlus(copy)
		}

	override def to[C1](factory :Factory[E, C1]) :C1 = sourceCollectionFactory(factory) match {
		case Yes(RelayArray | Seq | IndexedSeq | collection.Seq | collection.IndexedSeq) => this.asInstanceOf[C1]
		case Yes(ArraySeq) if length == unsafeArray.length                               =>
			ArraySeq.unsafeWrapArray(unsafeArray).castFrom[ArraySeq[Any], C1]
		case Yes(IRefArraySlice) if elementType == classOf[Any] =>
			IRefArraySlice.wrap(unsafeArray.castFrom[Array[_], IRefArray[E]]).castFrom[IRefArraySlice[E], C1]
		case Yes(IArrayLikeSlice | IArraySlice | ArrayLikeSlice) if elementType.isPrimitive =>
			IArraySlice.wrap(unsafeArray.castFrom[Array[_], IArray[E]]).castFrom[IArraySlice[E], C1]
		case _ => super.to(factory)
	}

	protected override def newSpecific(array :Array[E @uncheckedVariance], from :Int, until :Int) :RelayArray[E] =
		new RelayArrayPlus(array, from, until - from)

	private[this] def newArray(length :Int) :Array[E] = ArrayFactory.ofDim(elementType.castParam[E], length)

	private[this] def newArray[U >: E](cls :Class[U], length :Int) :Array[U] =
		if (cls.isConvertibleTo(elementType)) newArray(length).castFrom[Array[E], Array[U]]
		else ArrayFactory.ofDim(cls, length)

	private def writeReplace :Serializable = new ArraySerializationProxy[E](RelayArrayInternals.wrap(_), array)
}




/** The main, truly array backed, implementation of [[net.noresttherein.sugar.collections.RelayArray RelayArray]].
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
  * @see [[net.noresttherein.sugar.collections.RelayArrayView RelayArrayView]]
  */
@SerialVersionUID(Ver)
private final class RelayArrayPlus[@specialized(ElemTypes) +E] private[collections]
	                               (arr :Array[E], offset :Int, len :Int, owner :Boolean = false)
	extends AbstractSeq[E] with ProperRelayArray[E]
{
	private[collections] def this(array :Array[E]) = this(array, 0, array.length)
//
//	assert(offset >= 0, "negative offset " + offset + " out of " + arr.length)
//	assert(len >= 0, "negative length " + len)
//	assert(len <= arr.length - offset, "array[" + arr.length + "].slice(" + offset + ", " + (offset + len) + ")")

//	private[sugar] override def unsafeArray :Array[_] = array
	protected override def array :Array[E @uncheckedVariance] = arr
	private[sugar] override def startIndex :Int = offset
	@volatile private[this] var isOwner = owner //This assignment works as a memory fence ensuring visibility of the array.

	override def length :Int = len

	private def canPassOn :Boolean = {
		var read :Boolean = isOwner
		while (read && !OwnerField.weakCompareAndSet(this, true, false))
			read = isOwner
		read
	}

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			outOfBounds_!("RelayArray|" + len + "|(" + i + ")")
		else
			arr(offset + i)

	protected override def clippedSlice(from :Int, until :Int) :RelayArray[E] =
		until - from match {
			case 1 =>
				new RelayArray1(arr(offset + from))
			case 2 =>
				new RelayArray2(arr(offset + from), arr(offset + from + 1), elementType)
			case len if len < arr.length / SliceReallocationFactor =>
				val copy = ArrayFactory.ofDim(elementType, len).asInstanceOf[Array[E]]
				ArrayLike.copy(arr, offset + from, copy, 0, len)
				new RelayArrayPlus(copy)
			case len =>
				new RelayArrayPlus(arr, offset + from, len)
		}

	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def newInstance(array :Array[E], offset :Int, len :Int) =
		new RelayArrayPlus(array, offset, len, true)

	//todo: Move up canPassOn and appending/prepending methods up to ProperRelayArray
	override def appended[B >: E](elem :B) :RelayArray[B] = {
		val elemType           = elem.getClass
		val canStore           = elemType isConvertibleTo elementType
		var res :RelayArray[B] = null
		if (canStore)
			if (offset + len < arr.length) {
				if (canPassOn) {
					arr(offset + len) = elem.asInstanceOf[E]
					res = newInstance(arr, offset, len + 1)
				}
			} else if (len == 0 && arr.length > 0 && canPassOn) {
				arr(0) = elem.asInstanceOf[E]
				res = newInstance(arr, 0, 1)
			}
		if (res == null) {
			val halfSize = (len + 1) / 2
			val newOffset = math.min(offset, halfSize)
			val capacity = math.max(newOffset + len + halfSize, InitSize)
			val copy =
				if (canStore) ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[B]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
			copy(newOffset + len) = elem
			ArrayLike.copy(arr, offset, copy, newOffset, len)
			res =
				if (canStore) newInstance(copy.asInstanceOf[Array[E]], newOffset, len + 1)
				else new RelayArrayPlus(copy, newOffset, len + 1, true)
		}
		res
	}


	override def prepended[B >: E](elem :B) :RelayArray[B] = {
		val elemType           = elem.getClass
		val canStore           = elemType isConvertibleTo elementType
		var res :RelayArray[B] = null
		if (canStore)
			if (offset > 0) {
				if (canPassOn) {
					arr(offset - 1) = elem.asInstanceOf[E]
					res = newInstance(arr, offset - 1, len + 1)
				}
			} else if (len == 0 && arr.length > 0 && canPassOn) {
				val newOffset = arr.length - 1
				arr(newOffset) = elem.asInstanceOf[E]
				res = newInstance(arr, newOffset, 1)
			}
		if (res == null) {
			val halfSize = (len + 1) / 2
			val padding = math.min(arr.length - offset - len, halfSize)
			val capacity = math.max(halfSize + len + padding, InitSize)
			val newOffset = capacity - padding - len
			val copy =
				if (canStore) ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[B]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
			ArrayLike.copy(arr, offset, copy, newOffset, len)
			copy(newOffset - 1) = elem
			res =
				if (canStore) newInstance(copy.asInstanceOf[Array[E]], newOffset - 1, len + 1)
				else new RelayArrayPlus(copy, newOffset - 1, len + 1)
		}
		res
	}

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :RelayArray[B] =
		suffix match {
			case it :Iterable[B] if it.isEmpty  => this
			case it :Iterator[B] if !it.hasNext => this
			case it :RelayArray[B] if len == 0 && (it.length > arr.length || !isOwner) => it
			case it :RelayArray[B] =>
				val capacity           = arr.length
				val extras             = it.length
				val newSize            = len + extras
				val myType             = elementType
				val theirType          = it.elementType
				val canStore           = theirType isConvertibleTo myType
				var res :RelayArray[B] = null
				if (canStore)
					if (offset + newSize <= capacity) {
						if (canPassOn) {
							it.copyToArray(arr.asInstanceOf[Array[B]], offset + len, extras)
							res = newInstance(arr, offset, newSize)
						}
					} else if (len == 0 && extras <= capacity && canPassOn) {
						it.copyToArray(arr.asInstanceOf[Array[B]], 0, extras)
						res = newInstance(arr, 0, extras)
					}
				if (res == null) {
					val halfSize = (len + 1) / 2
					val newOffset = math.min(offset, halfSize)
					val capacity = math.max(newOffset + len + math.max(extras, halfSize), InitSize)
					val copy =
						if (canStore) ArrayFactory.ofDim(myType, capacity).asInstanceOf[Array[B]]
						else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
					ArrayLike.copy(arr, offset, copy, newOffset, len)
					it.copyToArray(copy, newOffset + len, extras)
					if (canStore)
						res = newInstance(copy.asInstanceOf[Array[E]], newOffset, newSize)
					else
						res = new RelayArrayPlus(copy, newOffset, newSize, true)
				}
				res
			case it :Iterable[B] => it.knownSize match {
				case -1 =>
					super.appendedAll(it)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :RelayArray[B] = null
						if (offset + newSize <= unsafeArray.length) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(arr.asInstanceOf[Array[B]], offset + len, newSize)
								res = newInstance(arr, offset, newSize)
							}
						}
						if (res == null) {
							val halfSize = (len + 1) / 2
							val newOffset = math.min(offset, halfSize)
							val capacity = math.max(newOffset + len + math.max(extras, halfSize), InitSize)
							val copy = ArrayFactory.ofDim(arr.getClass.getComponentType, capacity).asInstanceOf[Array[E]]
							it.copyToArray(copy.asInstanceOf[Array[B]], newOffset + len, newSize)
							ArrayLike.copy(arr, offset, copy, newOffset, len)
							res = newInstance(copy, newOffset, newSize)
						}
						res
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							if (wasOwner)
								isOwner = true
							val halfSize = (len + 1) / 2
							val newOffset = math.min(offset, halfSize)
							val capacity = math.max(newOffset + len + math.max(extras, halfSize), InitSize)
							val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
							it.copyToArray(copy, newOffset + len, newSize)
							ArrayLike.copy(unsafeArray, offset, copy, newOffset, len)
							new RelayArrayPlus(copy, newOffset, newSize, true)
					}
			}
			case _ => super.appendedAll(suffix)
		}


	override def prependedAll[B >: E](prefix :IterableOnce[B]) :RelayArray[B] =
		prefix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :RelayArray[B] if len == 0 && (it.length > arr.length || !isOwner) => it
			case it :RelayArray[B] =>
				val extras             = it.length
				val newSize            = len + extras
				val myType             = elementType
				val theirType          = it.elementType
				val canStore           = theirType isConvertibleTo myType
				var res :RelayArray[B] = null
				if (canStore)
					if (offset >= extras) {
						if (canPassOn) {
							it.copyToArray(arr.asInstanceOf[Array[B]], offset - extras, extras)
							res = newInstance(arr, offset - extras, newSize)
						}
					} else if (len == 0 && extras <= arr.length && canPassOn) {
						val newOffset = arr.length - extras
						it.copyToArray(arr.asInstanceOf[Array[B]], newOffset, extras)
						res = newInstance(arr, newOffset, extras)
					}
				if (res == null) {
					val halfSize = (len + 1) / 2
					val padding = math.min(unsafeArray.length - offset - len, halfSize)
					val capacity = math.max(math.max(extras, halfSize) + len + padding, InitSize)
					val newOffset = capacity - padding - len - extras
					val copy =
						if (canStore) ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[B]]
						else new Array[AnyRef](capacity).asInstanceOf[Array[B]]
					ArrayLike.copy(unsafeArray, offset, copy, newOffset + extras, len)
					it.copyToArray(copy, newOffset, extras)
					if (canStore)
						res = newInstance(copy.asInstanceOf[Array[E]], newOffset, newSize)
					else
						res = new RelayArrayPlus[B](copy, newOffset, newSize, true)
				}
				res
			case it :Iterable[B] => it.knownSize match {
				case -1 => super.prependedAll(prefix)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :RelayArray[B] = null
						if (offset >= extras) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(arr.asInstanceOf[Array[B]], offset - extras, extras)
								res = newInstance(arr, offset - extras, newSize)
							}
						}
						if (res == null) {
							val halfSize = (len + 1) / 2
							val padding = math.min(arr.length - offset - len, halfSize)
							val capacity = math.max(math.max(extras, halfSize) + len + padding, InitSize)
							val newOffset = capacity - padding - len - extras
							val copy = ArrayFactory.ofDim(arr.getClass.getComponentType, capacity).asInstanceOf[Array[E]]
							it.copyToArray(copy.asInstanceOf[Array[B]], newOffset, extras)
							ArrayLike.copy(arr, offset, copy, newOffset + extras, len)
							res = newInstance(copy, newOffset, newSize)
						}
						res
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							if (wasOwner) {
								isOwner = true
							}
							val halfSize = (len + 1) / 2
							val padding = math.min(unsafeArray.length - offset - len, halfSize)
							val capacity = math.max(math.max(extras, halfSize) + len + padding, InitSize)
							val newOffset = capacity - padding - len - extras
							val copy = new Array[AnyRef](capacity).asInstanceOf[Array[B]]
							it.copyToArray(copy, newOffset, extras)
							ArrayLike.copy(arr, offset, copy, newOffset + extras, len)
							new RelayArrayPlus(copy, newOffset, newSize, true)
					}
			}
			case _ => super.prependedAll(prefix)
		}


	private def fieldHandle =
		MethodHandles.lookup().findVarHandle(classOf[RelayArrayPlus[Any]],
//			"isOwner", classOf[Boolean]
			classOf[RelayArrayPlus[Any]].getName.replace('.', '$') + "$$isOwner", classOf[Boolean]
		)

}




/** An array backed implementation of [[net.noresttherein.sugar.collections.RelayArray PassedArray]]
  * which is not the owner of the underlying array (hence all appends and prepends will allocate a new array).
  * Additionally, slicing operations ''always'' return a view over the same array instance, never reallocating
  * the elements (unlike in [[net.noresttherein.sugar.collections.RelayArrayPlus PassedArrayPlus]].
  */
@SerialVersionUID(Ver)
private final class RelayArrayView[@specialized(ElemTypes) +E] private[collections]
	                              (arr :Array[E], offset :Int, len :Int)
	extends AbstractSeq[E] with ProperRelayArray[E]
{
	releaseFence()
	protected override def array :Array[E @uncheckedVariance] = arr
//	private[sugar] override def unsafeArray :Array[_] = array
	private[sugar] override def startIndex :Int = offset
	override def length :Int = len

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			outOfBounds_!("RelayArray|" + len + "|(" + i + ")")
		else
			arr(offset + i)

	override def safe :RelayArray[E] = {
		val copy = ArrayFactory.ofDim(elementType, len).asInstanceOf[Array[E]]
		ArrayLike.copy(arr, offset, copy, 0, len)
		new RelayArrayPlus(arr, 0, len)
	}

	override def slice(from :Int, until :Int) :RelayArray[E] = window(from, until)
	protected override def clippedSlice(from :Int, until :Int) :RelayArray[E] = window(from, until)
	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows
	//extracted for specialization
	private[this] def passedArray(array :Array[E], offset :Int, len :Int) =
		new RelayArrayPlus(array, offset, len, true)


	override def appended[B >: E](elem :B) :RelayArray[B] = {
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		if (canStore) {
			val copy = ArrayFactory.ofDim(elementType, len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			ArrayLike.copy(arr, offset, copy, 0, len)
			passedArray(copy, 0, len + 1)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			ArrayLike.copy(arr, offset, copy, 0, len)
			new RelayArrayPlus(copy, 0, len + 1)
		}
	}


	override def prepended[B >: E](elem :B) :RelayArray[B] = {
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		if (canStore) {
			val copy = ArrayFactory.ofDim(elementType, len + 1).asInstanceOf[Array[E]]
			ArrayLike.copy(arr, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			passedArray(copy, 0, len + 1)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			ArrayLike.copy(arr, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			new RelayArrayPlus(copy, 0, len + 1)
		}
	}

	override def appendedAll[B >: E](suffix :IterableOnce[B]) :RelayArray[B] =
		suffix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :RelayArray[B] if len == 0 => it
			case it :RelayArray[B] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				if (canStore) {
					val copy = ArrayFactory.ofDim(myType, len + extras).asInstanceOf[Array[B]]
					ArrayLike.copy(arr, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					passedArray(copy.asInstanceOf[Array[E]], 0, copy.length)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[B]]
					ArrayLike.copy(arr, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					new RelayArrayPlus(copy, 0, copy.length)
				}
			case _ => super.appendedAll(suffix)
		}


	override def prependedAll[B >: E](prefix :IterableOnce[B]) :RelayArray[B] =
		prefix match {
			case it :Iterable[B] if it.isEmpty => this
			case it :Iterator[B] if !it.hasNext => this
			case it :RelayArray[B] if len == 0 => it
			case it :RelayArray[B] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				if (canStore) {
					val copy = ArrayFactory.ofDim(elementType, len + extras).asInstanceOf[Array[B]]
					ArrayLike.copy(unsafeArray, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					passedArray(copy.asInstanceOf[Array[E]], 0, len + extras)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[B]]
					ArrayLike.copy(unsafeArray, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					new RelayArrayPlus(copy.asInstanceOf[Array[E]], 0, len + extras)
				}
			case _ => super.prependedAll(prefix)
		}

}






/** $factoryInfo
  *
  * Despite $Coll being backed by an array,
  * this is not an [[scala.collection.EvidenceIterableFactory EvidenceIterableFactory]].
  * Instead, it takes an optimistic approach to specialization: if elements added to a `RelayArray`, its builder,
  * or passed as arguments to any of factory methods of this object are all of a built in value type,
  * a proper primitive array is allocated. At the same time, it guards itself against type errors which would
  * result in a [[ClassCastException]]/[[ArrayStoreException]], and reallocates the underlying array as an
  * `Array[`[[Any]]`]` if needed.
  * @define Coll `RelayArray`
  * @define coll relay array
  */
@SerialVersionUID(Ver)
case object RelayArray extends StrictOptimizedSeqFactory[RelayArray] {

	override def from[E](it :IterableOnce[E]) :RelayArray[E] = it match {
		case elems :RelayArray[E] =>
			elems
		case elems :Iterable[E] if elems.knownSize == 0 =>
			Empty
		case elems :View[E] =>
			//Views delegate isEmpty to iterator.isEmpty, but are common arguments,
			// so there is no sense in creating the iterator twice.
			new RelayArrayPlus(elems.toArray(classTag[AnyRef].asInstanceOf[ClassTag[E]]))
		case elems :Iterable[E] if elems.isEmpty => //usually, it should be also knownSize == 0, but it's not guaranteed.
			Empty
		case elems :Iterable[E] if elems.sizeIs == 1 =>
			new RelayArray1(elems.head)
		case elems :ArraySeq[E] =>
			RelayArrayInternals.make(elems.unsafeArray, 0, elems.length).castParam[E]
		//todo: IArray extractor not requiring a ClassTag
//		case IArray.Wrapped.Slice(array, from, until) =>
//			RelayArrayInternals.make(array.castFrom[IArray[_], Array[E]], from, until - from)
		case IRefArray.Wrapped.Slice(array, from, until) =>
			new RelayArrayPlus(array.castFrom[IRefArray[_], Array[E]], from, until - from)
		case ErasedArray.Wrapped.Slice(array, from, until) =>
			RelayArrayInternals.make(array.slice(from, until), 0, until - from).castParam[E]
		case elems :Iterable[E] => elems.head.getClass match {
			case PrimitiveClass(valueClass) => try {
				RelayArrayInternals.wrap(elems.toArray(ClassTag(valueClass).castParam[E]))
			} catch {
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					new RelayArrayPlus(elems.toArray(ClassTag.Any.castParam[E]))
			}
			case _                    =>
				new RelayArrayPlus(elems.toArray(ClassTag.Any.castParam[E]))
		}
		case _ =>
			val i = it.iterator
			if (!i.hasNext) Empty
			else new RelayArrayPlus(i.toArray(classTag[AnyRef].asInstanceOf[ClassTag[E]]))
	}

	/** A `RelayArray` containing the elements of the given array. If the array has two or fewer elements,
	  * a dedicated instance is returned, not backed by an array. Otherwise, the array is copied and wrapped.
	  */
	def from[E](array :Array[E]) :RelayArray[E] = array.length match {
		case 0 => Empty
		case 1 => new RelayArray1(array(0))
		case 2 =>
			((array :Array[_]) match {
				case a :Array[AnyRef]  => new RelayArray2(a(0), a(1))
				case a :Array[Int]     => new RelayArray2(a(0), a(1))
				case a :Array[Long]    => new RelayArray2(a(0), a(1))
				case a :Array[Double]  => new RelayArray2(a(0), a(1))
				case a :Array[Byte]    => new RelayArray2(a(0), a(1))
				case a :Array[Char]    => new RelayArray2(a(0), a(1))
				case a :Array[Float]   => new RelayArray2(a(0), a(1))
				case a :Array[Short]   => new RelayArray2(a(0), a(1))
				case a :Array[Boolean] => new RelayArray2(a(0), a(1))
			}).castParam[E]
		case n => new RelayArrayPlus(Array.copyOf(array, n))
	}

	def slice[E](array :Array[E], from :Int, until :Int) :RelayArray[E] =
		RelayArrayInternals.wrap(array.slice(from, until))

	implicit def wrap[E](array :IArray[E]) :RelayArray[E] = {
		val a = array.castFrom[IArray[E], Array[E]]
		RelayArrayInternals.make(a, 0, a.length)
	}

	def view[E](array :IArray[E], from :Int, until :Int) :RelayArray[E] =
		RelayArrayInternals.slice(array.castFrom[IArray[E], Array[E]], from, until)


	override def empty[E] :RelayArray[E] = Empty

	@inline def :+[@specialized(ElemTypes) E](elem :E) :RelayArray[E] = single(elem)

	@inline def :++[@specialized(ElemTypes) E](elems :IterableOnce[E]) :RelayArray[E] = from(elems)

	/** A $Coll singleton */
	def single[@specialized(ElemTypes) E](elem :E) :RelayArray[E] = new RelayArray1(elem)

	/** A $Coll singleton (same as [[net.noresttherein.sugar.collections.RelayArray.single single]]). */
	def one[@specialized(ElemTypes) E](elem :E) :RelayArray[E] = single(elem)

	/** A special $Coll of only two elements. */
	def two[@specialized(ElemTypes) A](first :A, second :A) :RelayArray[A] = {
		val firstClass = first.getClass.unwrapped
		val secondClass = second.getClass.unwrapped
		if (firstClass != secondClass)
			new RelayArray2(first, second)
		else //only for as long as it is specialized
			(if (classOf[AnyRef].isAssignableFrom(firstClass))
				new RelayArray2(first, second, classOf[AnyRef])
			else if (firstClass == classOf[Int])
				new RelayArray2(first.asInstanceOf[Int], second.asInstanceOf[Int], classOf[Int])
			else if (firstClass == classOf[Long])
				new RelayArray2(first.asInstanceOf[Long], second.asInstanceOf[Long], classOf[Long])
			else if (firstClass == classOf[Double])
				new RelayArray2(first.asInstanceOf[Double], second.asInstanceOf[Double], classOf[Double])
			else if (firstClass == classOf[Byte])
				new RelayArray2(first.asInstanceOf[Byte], second.asInstanceOf[Byte], classOf[Byte])
			else if (firstClass == classOf[Char])
				new RelayArray2(first.asInstanceOf[Char], second.asInstanceOf[Char], classOf[Char])
			else if (firstClass == classOf[Float])
				new RelayArray2(first.asInstanceOf[Float], second.asInstanceOf[Float], classOf[Float])
			else if (firstClass == classOf[Short])
				new RelayArray2(first.asInstanceOf[Short], second.asInstanceOf[Short], classOf[Short])
			else if (firstClass == classOf[Boolean])
				new RelayArray2(first.asInstanceOf[Boolean], second.asInstanceOf[Boolean], classOf[Boolean])
			else
				new RelayArray2(first, second, classOf[Any])
			).castParam[A]
	}

	/** An empty $Coll backed by the array of the specified length. Similarly to the capacity argument
	  * to [[scala.collection.mutable.ArrayBuffer ArrayBuffer]] and [[StringBuilder]], it allows to avoid
	  * repeated reallocations if the future size is known, at least approximately. Each initial append/prepend
	  * will create only a lightweight object, backed by the array created here, for as long as its capacity
	  * is not exceeded.
	  */
	def ofCapacity[E](capacity :Int) :RelayArray[E] = capacity match {
		case 0 => empty
		case n => new RelayArrayPlus[E](new Array[Any](n).castParam[E], 0, 0, true)
	}

	/** An optimistic `RelayArray` builder; if the first element added is a value type (or, rather, a Java box
	  * for a Java primitive type), it allocates an array of the appropriate value type. The classes of subsequently
	  * added elements are checked until a first element with a type not fitting in the array is found,
	  * in which case the underlying array is reallocated as `Array[AnyRef]`.
	  * It is somewhat slower than [[net.noresttherein.sugar.collections.RelayArray.genericBuilder genericBuilder]],
	  * especially when adding individual elements, but considerably faster when adding `Array`-backed collections:
	  * [[collection.immutable.ArraySeq immutable.ArraySeq]], [[collection.mutable.ArraySeq mutable.ArraySeq]]
	  * and `RelayArray`.
	  */
	override def newBuilder[E] :Builder[E, RelayArray[E]] = //newBuilder(classOf[AnyRef].asInstanceOf[Class[A]])
		new OptimisticBuilder[E](null)

	/** A builder of a `RelayArray` backed by an `Array[AnyRef]`. More efficient for reference types than
	  * [[net.noresttherein.sugar.collections.RelayArray.newBuilder newBuilder]].
	  */
	def genericBuilder[E] :Builder[E, RelayArray[E]] =
		newBuilder(classOf[AnyRef].castParam[E])

	/** A builder for a $Coll backed by an array with a specific element type, defined by the implicit [[ClassTag]]. */
	def specificBuilder[E :ClassTag] :Builder[E, RelayArray[E]] =
		newBuilder(classTag[E].runtimeClass.asInstanceOf[Class[E]])

	/** A builder for a $Coll backed by an array with the specified element type. */
	def newBuilder[E](elementType :Class[E]) :Builder[E, RelayArray[E]] =
		new SpecificBuilder(elementType)

	private class SpecificBuilder[E](elemType :Class[E]) extends ReusableBuilder[E, RelayArray[E]] {
		private[this] var elems :Array[E] = _
		private[this] var size            = 0
		private[this] var initSize        = InitSize
		override def knownSize = size
		protected def knownSize_=(size :Int) :Unit = this.size = size
		protected def elementType = elemType

		def capacity :Int = if (elems == null) 0 else elems.length

		private def newArray(size :Int) :Array[E] =
			if (elementType == null)
				ArrayFactory.ofDim(classOf[Any], size).asInstanceOf[Array[E]]
			else
				ArrayFactory.ofDim(elementType, size)

		protected final def realloc(newSize :Int) :Unit =
			elems = ArrayFactory.copyOf(elems, elementType, newSize)

		override def sizeHint(size :Int) :Unit =
			if (size > 0)
				if (elementType == null)
					initSize = size
				else if (elems == null)
					elems = newArray(size)
				else if (size > elems.length)
					realloc(size)

		final def ensure(extras :Int) :Unit = {
			if (elems == null)
				elems = newArray(math.max(extras, initSize))
			else {
				val capacity = elems.length
				val newSize = size + extras
				if (newSize > capacity) {
					val newCapacity = math.max(capacity * 2, newSize)
					if (initSize > newSize)
						realloc(math.min(newCapacity, initSize))
					else
						realloc(newCapacity)
				}
			}
		}

		override def addAll(xs :IterableOnce[E]) :this.type = xs match {
			case it :Iterable[E] if it.knownSize == 0 => this
			case it :Iterator[E] if !it.hasNext => this
			case it :Iterable[E] => it.knownSize match {
				case -1 => super.addAll(it)
				case n  =>
					ensure(n)
					assert(it.copyToArray(elems, size) == n)
					size += n
					this
			}
			case _ => super.addAll(xs)
		}

		override def addOne(elem :E) = {
			if (elems == null)
				elems = newArray(initSize)
			else if (size == elems.length)
				elems = Array.copyOf(elems, size * 2)
			elems(size) = elem
			size += 1
			this
		}

		override def clear() :Unit = {
			elems    = null
			size     = 0
			initSize = InitSize
		}

		override def result() = size match {
			case 0 => empty
			case 1 => new RelayArray1(elems(0))
			case 2 => new RelayArray2(elems(0), elems(1))
			case n if n < elems.length * AcceptableBuilderFillRatio =>
				val res = new RelayArrayPlus(Array.copyOf(elems, n))
				clear()
				res
			case _ =>
				val res = new RelayArrayPlus(Array.copyOf(elems, size))
				clear()
				res
		}
	}


	private final class OptimisticBuilder[E](private[this] var _elementType :Class[E])
		extends SpecificBuilder[E](_elementType)
	{
		private[this] var optimistic = _elementType == null || elementType != classOf[Any]
		//overridden because we use it much more, so want to use a field and not a method call.
		override def elementType :Class[E] = _elementType
		def elementType_=(tpe :Class[E]) :Unit = _elementType = tpe

		override def addOne(elem :E) :this.type = {
			if (_elementType == null) {
				_elementType = elem.getClass.castParam[E]
				if (!_elementType.isPrimitive)
					_elementType = classOf[Any].castParam[E]
				super.addOne(elem)
			} else if (optimistic)
				if (elem.getClass isConvertibleTo _elementType)
					super.addOne(elem)
				else {
					optimistic = false
					_elementType = classOf[AnyRef].castParam[E]
					realloc(math.max(capacity, InitSize))
					super.addOne(elem)
				}
			else
				super.addOne(elem)
		}
		override def addAll(xs :IterableOnce[E]) =
			xs match {
				case _ if !optimistic => super.addAll(xs)
				case empty :Iterable[_] if empty.isEmpty => this
				case empty :Iterator[_] if !empty.hasNext => this
				case items :ArrayIterableOnce[E] => addKnownType(items.unsafeArray.getClass.getComponentType, items)
				case items :ArraySeq[E]           => addKnownType(items.unsafeArray.getClass.getComponentType, items)
				case items :mutable.ArraySeq[E]   => addKnownType(items.array.getClass.getComponentType, items)
				case items :Iterable[E]           => addKnownType(items.head.getClass, items)
				case _ =>
					val iter = xs.iterator
					while (iter.hasNext)
						addOne(iter.next())
					this
			}
		private def addKnownType(elemType :Class[_], xs :IterableOnce[E]) :this.type =
			if (_elementType == null) {
				_elementType = (if (elemType.isPrimitive) elemType else classOf[AnyRef]).castParam[E]
				addOptimistic(xs)
			} else if (!elemType.isConvertibleTo(_elementType)) {
				optimistic = false
				val size = knownSize
				val extras = xs.knownSize
				val newSize =
					if (extras >= 0) math.max(math.min(size + extras, size * 2), InitSize)
					else if (size == capacity) math.max(size * 2, InitSize)
					else size
				elementType = classOf[AnyRef].asInstanceOf[Class[E]]
				realloc(newSize)
				super.addAll(xs)
			} else
				addOptimistic(xs)

		private def addOptimistic(xs :IterableOnce[E]) :this.type = xs match {
			case _ :Iterable[E] =>
				val rollbackSize = knownSize
				try { super.addAll(xs) } catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						optimistic = false
						_elementType = classOf[Any].castParam[E]
						knownSize = rollbackSize
						realloc(capacity)
						super.addAll(xs)
				}
			case _ =>
				val i = xs.iterator
				while (i.hasNext)
					addOne(i.next())
				this
		}
	}


	private[this] final val Empty = new RelayArray0

	override def toString = "RelayArray"
}


private object RelayArrayPlus {
	private[collections] val OwnerField = new RelayArrayPlus(Array.emptyIntArray).fieldHandle
}


//todo: make ProperRelayArray wrap IArrayLike, move the wrapping method to RelayArray object, and remove this class.
private object RelayArrayInternals
	extends ArrayLikeSliceWrapper[RelayArray, Array] with StrictOptimizedSeqFactory[RelayArray]
{
	override def make[E](array :Array[E], from :Int, until :Int) :RelayArray[E] =
		((array :Array[_]) match {
			case arr :Array[AnyRef]  => new RelayArrayPlus(arr, from, until)
			case arr :Array[Int]     => new RelayArrayPlus(arr, from, until)
			case arr :Array[Long]    => new RelayArrayPlus(arr, from, until)
			case arr :Array[Double]  => new RelayArrayPlus(arr, from, until)
			case arr :Array[Byte]    => new RelayArrayPlus(arr, from, until)
			case arr :Array[Char]    => new RelayArrayPlus(arr, from, until)
			case arr :Array[Float]   => new RelayArrayPlus(arr, from, until)
			case arr :Array[Short]   => new RelayArrayPlus(arr, from, until)
			case arr :Array[Boolean] => new RelayArrayPlus(arr, from, until)
//			case arr :Array[Unit]    => new PassedArrayPlus(arr, from, until)
			case _ => new RelayArrayPlus(array, from, until)
		}).castParam[E]

	def superElementType(elem1 :Any, elem2 :Any) :Class[_] =
		if (elem1 == null | elem2 == null)
			classOf[Any]
		else
			superElementType(elem1.getClass, elem2.getClass)

	def superElementType(type1 :Class[_], type2 :Class[_]) :Class[_] = {
		val unboxed1 = Unboxed(type1)
		val unboxed2 = Unboxed(type2)
		if (unboxed1.isAssignableFrom(unboxed2)) unboxed1
		else if (unboxed2.isAssignableFrom(unboxed1)) unboxed2
		else classOf[AnyRef]
	}

	final val InitSize = 8
	final val SliceReallocationFactor = 4 //slices smaller than 1/4 reallocate the backing array.
	final val AcceptableBuilderFillRatio = 0.75
	final val applyPreferredMaxLength = Int.MaxValue
	val OwnerField = RelayArrayPlus.OwnerField
//		MethodHandles.lookup().findVarHandle(classOf[RelayArrayPlus[Any]],
//			"isOwner", classOf[Boolean]
//			classOf[RelayArrayPlus[Any]].getName.replace('.', '$') + "$$isOwner", classOf[Boolean]
//		)
	override def isImmutable = true


	override def from[A](source :IterableOnce[A]) :RelayArray[A] = RelayArray.from(source)
	override def empty[A] :RelayArray[A] = RelayArray.empty
	override def newBuilder[A] :Builder[A, RelayArray[A]] = RelayArray.newBuilder
}
