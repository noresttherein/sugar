package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.lang.constant.Constable
import java.lang.invoke.MethodHandles

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized
import scala.collection.{Factory, IterableFactoryDefaults, SeqFactory, Stepper, StepperShape, StrictOptimizedSeqFactory, View, mutable}
import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}
import scala.reflect.{ClassTag, classTag}

import net.noresttherein.sugar.arrays.{ArrayFactory, ArrayLike, ErasedArray, IArray, IArrayLike, IRefArray, TypedArray, arraycopy}
import net.noresttherein.sugar.collections.CompanionFactory.sourceCollectionFactory
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.collections.HasFastSlice.preferDropOverIterator
import net.noresttherein.sugar.collections.RelayArrayPlus.{AcceptableBuilderFillRatio, InitSize, OwnerField, SliceReallocationFactor, superElementType}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.{illegalState_!, illegal_!, maxSize_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.numeric.extensions.IntExtension
import net.noresttherein.sugar.reflect.{Boxed, PrimitiveClass, Unboxed, classes}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}

//implicits
import net.noresttherein.sugar.extensions.{ArrayCompanionExtension, classNameMethods, castTypeParamMethods, ClassExtension, IterableExtension, IterableOnceExtension, IteratorCompanionExtension, castingMethods}




//Consider: a supertype for RelayArrayRange, ReversedSeq, SeqSlice, etc. AdapterSeq, Subcollection?
//todo: introduce a supertype, and make RelayArrayRange a subtype of it, apart from other subclasses.
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
  * The maximum sequence length is limited by the maximum permissible array size. The latter may differ between
  * JVMs, so, for consistent behaviour, it is explicitly limited by the implemetation to $MaxSize.
  * Note however, that the offset at which the elements are placed in the underlying array may change
  * with reallocation; in general, appending elements to either end of the array will reduce unused space
  * at the other end on array reallocation if it exceeds a certain factor. This means that, in particular,
  * the contents will be shifted to accomodate new elements, as long as the total size does not exceed
  * the aforementioned limit. If this is not sufficient to make room for the appended/prepended elements,
  * a [[net.noresttherein.sugar.exceptions.MaxSizeReachedException MaxSizeReachedException]] is thrown.
  * Very short sequences have dedicated implementations, without a backing array.
  *
  * Tip: unlike `ArraySeq`, `RelayArray` doesn't offer a method exposing the underlying array,
  * but it can still be extracted by matching it to
  * [[net.noresttherein.sugar.arrays.IArrayLike IArrayLike]]`.``.`[[net.noresttherein.sugar.arrays.IArrayLike.Slice Slice]].
  * @define Coll    `RelayArray`
  * @define coll    relay array
  * @define MaxSize `Int.MaxValue - 8`
  * @author Marcin Mościcki
  */ //todo: BooleanRelayArray implementation;
	// todo: make RelayArray1 and RelayArray2 only a RelaySeq, and ProprRelayArray the new RelayArray.
//fixme: the docs mention that RelayArray(1, 2, 3) is backed by an Array[Int], which is not currently the case.
sealed trait RelayArray[@specialized(ElemTypes) +E]
	extends IndexedSeq[E] with IndexedSeqOps[E, RelayArray, RelayArray[E]]
	   with StrictOptimizedSeqOps[E, RelayArray, RelayArray[E]] with IterableFactoryDefaults[E, RelayArray]
	   with SugaredIterable[E] with SugaredSeqOps[E, RelayArray, RelayArray[E]] with Serializable
{
	override def iterableFactory :SeqFactory[RelayArray] = RelayArray
	protected override def className = "RelayArray"
	def elementType :Class[_]

	/** A $Coll being a view on this instance. Calling `slice` on the result will never allocate a new array
	  * to copy the contents, even if the view is a minor fragment of the underlying array
	  * (`(until - from)/length` is large). Adding elements to the view, mapping or any other operations
	  * which do not return a continuous subsequence of this sequence will create a normal, non view instance.
	  * This makes `tail` and other slicing operations very fast, and the implementation particularly suited towards
	  * list-like processing. Due to the possibility of inducing a memory leak, this implementation should be used
	  * only by code which is certain not to expose any of the slices back to the application.
	  *
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collections.RelayArray.downsize downsize]] creates a copy with normal
	  * shrinking behaviour. The result may be a dedicated class if new size is very small.
	  * @return [[net.noresttherein.sugar.collections.RelayArray.range(from:Int* window]]`(0, length)`.
	  */ //consider: renaming to subseq or range
	def temp :RelayArray[E] = range(0, length)

	/** A $Coll being a view on this instance. Equivalent to `slice`, but no new array is allocated
	  * even if the view is a minor fragment of the underlying array (`(until - from)/length` is large).
	  * Additionally, all `slice` operations on the result are also views on this array, without any reallocation.
	  * Adding elements to the view will create a normal, non view instance. This makes `tail` and other
	  * slicing operations very fast, and the implementation particularly suited towards list-like processing.
	  * Due to the possibility of inducing a memory leak, this implementation should be used only by code which
	  * is certain not to expose any of the slices back to the application.
	  *
	  * In case a conversion to a non view implementation is needed,
	  * [[net.noresttherein.sugar.collections.RelayArray.downsize downsize]] creates a copy with normal
	  * shrinking behaviour. The result may be a dedicated class if new size is very small.
	  */
	def range(from :Int, until :Int) :RelayArray[E]

	/** A version of this sequence guarded against memory leaks caused by slicing.
	  * The default implementation is already safe (returns `this`); this method is used in conjunction
	  * with [[net.noresttherein.sugar.collections.RelayArray.range range]], which creates
	  * a view over the underlying array which never copies elements while slicing, regardless of the ratio
	  * between the slice length and the underlying array length.
	  */ //consider: renaming to shrinking, frugal
	def downsize :RelayArray[E] = this

	/** Reallocates (if needed) the data to a new array of an appropriate size to free the larger array
	  * for garbage collection (unless used by another instance).
	  */ //consider: renaming to pack or compact
	def shrink :RelayArray[E] = this

	//specialized
	protected override def newSpecificBuilder :Builder[E @uncheckedVariance, RelayArray[E]] =
		RelayArray.newBuilder[E](elementType.asInstanceOf[Class[E]])

	@unspecialized protected def newInstance[U >: E](array :Array[U], offset :Int, length :Int) :RelayArray[U] =
		RelayArray.make(array.asInstanceOf[IArray[U]], offset, offset + length)

	protected override def applyPreferredMaxLength :Int = RelayArrayPlus.applyPreferredMaxLength
}



/** An empty `RelayArray`. */
@SerialVersionUID(Ver)
private class RelayArray0
	extends AbstractSeq[Nothing] with RelayArray[Nothing] with EmptyIndexedSeqOps.Generic[RelayArray]
{
	protected override def one[T](elem :T) :RelayArray[T] = RelayArray.one(elem)
	override def elementType = classOf[AnyRef]
	override def range(from :Int, until :Int) :this.type = this
}



private trait RelayArrayPrivates[@specialized(ElemTypes) +E] extends RelayArray[E] {
	//extracted for specialization.
	protected[this] def seq1(elem :E) = new RelayArray1[E](elem)
	protected[this] def seq2(elem1 :E, elem2 :E) = new RelayArray2[E](elem1, elem2)

	protected[this] def newSpecific(array :Array[E], from :Int, until :Int, owner :Boolean) :RelayArray[E] = {
		val len = until - from
		if (len < array.length / SliceReallocationFactor)
			new RelayArrayPlus(array.slice(from, until), 0, len, false)
		else
			new RelayArrayPlus[E](array, from, until - from, owner)
	}

	protected def newArray(length :Int) :Array[E @uncheckedVariance] =
		ArrayFactory.ofDim(elementType.castParam[E], length)

	protected def newArray[U >: E](tpe :Class[_], length :Int) :Array[U] =
		if (Unboxed(elementType) isAssignableFrom Unboxed(tpe)) newArray(length).asInstanceOf[Array[U]]
		else new Array[Any](length).asInstanceOf[Array[U]]

	override def cyclicCopyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		cyclicCopyRangeToArray(xs, start, 0, len)

	override def copyToArray[A >: E](xs :Array[A], start :Int, len :Int) :Int =
		copyRangeToArray(xs, start, 0, len)

}


/** A [[net.noresttherein.sugar.collections.RelayArray RelayArray]] with a single element. */
@SerialVersionUID(Ver)
private class RelayArray1[@specialized(ElemTypes) +E] private[collections](override val head :E)
	extends AbstractSeq[E] with RelayArrayPrivates[E] with SingletonIndexedSeqOps.Generic[E, RelayArray]
{
	protected override def one[T](elem :T) :RelayArray[T] = RelayArray.one(elem)
	protected override def two[T](first :T, second :T) :RelayArray[T] = RelayArray.two(first, second)

	override def elementType = Unboxed(head.getClass)

	override def range(from :Int, until :Int) :RelayArray[E] = slice(from, until)

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
		try seq2(head, elem.asInstanceOf[E]) catch {
			case _ :ClassCastException => new RelayArray2[U](head, elem)
		}
	override def prepended[U >: E](elem :U) :RelayArray[U] =
		try seq2(elem.asInstanceOf[E], head) catch {
			case _ :ClassCastException => new RelayArray2[U](elem, head)
		}

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] = suffix match {
		case items :Iterable[U] if items.isEmpty => this
		case items :Iterator[U] if !items.hasNext => this
		case items :RelayArray1[U] =>
			try seq2(head, items.head.asInstanceOf[E]) catch {
				case _ :ClassCastException =>
					val elem = items.head
					new RelayArray2[U](head, elem)
			}
		case items :RelayArray[U] => this ++: items
		case items :Iterable[U] => items.knownSize match {
			case 1 =>
				try seq2(head, items.head.asInstanceOf[E]) catch {
					case _ :ClassCastException =>
						val elem = items.head
						new RelayArray2[U](head, elem)
				}
			case n => appendedAllLarge(items, n)
		}
		case _ =>
			val i = suffix.iterator
			i.knownSize match {
				case 1 =>
					try seq2(head, i.next().asInstanceOf[E]) catch {
						case _ :ClassCastException =>
							val elem = i.next()
							new RelayArray2[U](head, elem)
					}
				case n => appendedAllLarge(i, n)
			}
	}

	private def appendedAllLarge[U >: E](items :IterableOnce[U], knownSize :Int) =
		knownSize match {
			case -1 =>
				try
					(newSpecificBuilder += head ++= items.asInstanceOf[Iterable[E]]).result()
				catch { //ClassCastException in case the builder becomes specialized.
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						(RelayArray.genericBuilder[U] += head ++= items).result()
				}
			case  n =>
				try {
					val capacity = math.max(n + 1, InitSize)
					val array = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
					items match {
						case it :Iterable[E @unchecked] => it.copyToArray(array, 1)
						case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, 1)
					}
					array(0) = head
					newSpecific(array, 0, 1 + n, true)
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						val array = new Array[AnyRef](math.max(1 + n, InitSize)).asInstanceOf[Array[U]]
						items match {
							case it :Iterable[U] => it.copyToArray(array, 1)
							case _ => items.iterator.copyToArray(array, 1)
						}
						array(0) = head
						new RelayArrayPlus[U](array, 0, 1 + n, true)
				}
		}


	override def prependedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] = suffix match {
		case items :Iterable[U] if items.isEmpty  => this
		case items :Iterator[U] if !items.hasNext => this
		case items :RelayArray1[U] =>
			try seq2(items.head.asInstanceOf[E], head) catch {
				case _ :ClassCastException =>
					new RelayArray2[U](items.head, head)
			}
		case items :RelayArray[U] => items :++ this
		case items :Iterable[U] => items.knownSize match {
			case 1 =>
				try seq2(items.head.asInstanceOf[E], head) catch {
					case _ :ClassCastException =>
						val elem = items.head
						new RelayArray2[U](elem, head)
				}
			case n => prependedAllLarge(items, n)
		}
		case _ =>
			val i = suffix.iterator
			i.knownSize match {
				case 1 =>
					try seq2(i.next().asInstanceOf[E], head) catch {
						case _ :ClassCastException =>
							val elem = i.next()
							new RelayArray2[U](elem, head)
					}
				case n => prependedAllLarge(i, n)
			}
	}

	private def prependedAllLarge[U >: E](items :IterableOnce[U], knownSize :Int) =
		knownSize match {
			case -1 =>
				try
					(newSpecificBuilder ++= items.asInstanceOf[Iterable[E]] += head).result()
				catch { //ClassCastException in case the builder becomes specialized
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						(RelayArray.genericBuilder[U] ++= items += head).result()
				}
			case  n =>
				try {
					val newSize = n + 1
					val capacity = math.max(newSize, InitSize)
					val array = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
					items match {
						case it :Iterable[E @unchecked] => it.copyToArray(array, capacity - newSize)
						case _ => items.iterator.asInstanceOf[Iterator[E]].copyToArray(array, capacity - newSize)
					}
					array(capacity - 1) = head
					newSpecific(array, capacity - newSize, capacity, true)
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
						new RelayArrayPlus[U](array, capacity - newSize, newSize, true)
				}
		}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] =
		elems match {
			case _ if index < 0 | index > 1          => outOfBounds("insertedAll", index)
			case _ if elems.knownSize == 0           => this
			case view  :View[U]                      => insertedAll(index, view.iterator)
			case items :Iterable[U] if items.isEmpty => this
			case iter  :Iterator[U] if !iter.hasNext => this
			case _ if index == 0                     => RelayArray.from(elems) :+ head
			case _ =>
				val res = RelayArray.newBuilder[U]
				res.sizeHint(elems, 1)
				(res += head ++= elems).result()
		}

	override def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from > 0 || xs.length <= start)
			0
		else
			try { copy(xs.asInstanceOf[Array[E]], start); 1 } catch { //xs.asInstanceOf[U] may throw an exception?
				case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
					xs(start) = head; 1
			}

	override def cyclicCopyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
		if (xs.length == 0) 0
		else copyRangeToArray(xs, start % xs.length, from, len)

	private def copy(xs :Array[E @uncheckedVariance], start :Int) :Unit = xs(start) = head
}




/** A [[net.noresttherein.sugar.collections.RelayArray RelayArray]] of two elements. */
@SerialVersionUID(Ver)
private final class RelayArray2[@specialized(ElemTypes) +E] private[collections]
	                           (override val head :E, override val last :E, override val elementType :Class[_])
	extends AbstractSeq[E] with RelayArrayPrivates[E] with SeqSlicingOps[E, RelayArray, RelayArray[E]]
{
	def this(head :E, last :E) = this(head, last, superElementType(head, last))
	override def iterator = Iterator.two(head, last)
	override def reverseIterator = Iterator.two(last, head)

	override def jterator[I <: Jterator[_]](implicit shape :JteratorShape[E, I]) :I =
		Jterator.two(head, last)

	override def stepper[S <: Stepper[_]](implicit shape :StepperShape[E, S]) :S with EfficientSplit =
		Stepper2(head, last)

	override def length = 2

	override def apply(i :Int) :E = i match {
		case 0 => head
		case 1 => last
		case _ => outOfBounds_!("RelayArray|2|(" + i + ")")
	}

	override def range(from :Int, until :Int) :RelayArray[E] = slice(from, until)

	protected override def clippedSlice(from :Int, until :Int) :RelayArray[E] = from match {
		case 0 => new RelayArray1[E](head) //because 0 < until - from < 2
		case _ => new RelayArray1[E](last)
	}

	override def filter(pred :E => Boolean) :RelayArray[E] =
		if (pred(head))
			if (pred(last)) this else new RelayArray1[E](head)
		else
			if (pred(last)) new RelayArray1[E](last) else RelayArray.empty

	override def filterNot(pred :E => Boolean) :RelayArray[E] =
		if (pred(head))
			if (pred(last)) RelayArray.empty else new RelayArray1[E](last)
		else
			if (pred(last)) new RelayArray1[E](head) else this

	override def partition(p :E => Boolean) :(RelayArray[E], RelayArray[E]) =
		if (p(head))
			if (p(last)) (this, RelayArray.empty) else (new RelayArray1[E](head), new RelayArray1[E](last))
		else
			if (p(last)) (new RelayArray1[E](last), new RelayArray1[E](head)) else (RelayArray.empty, this)

	override def removed(index :Int) :RelayArray[E] = index match {
		case 0 => new RelayArray1[E](last)
		case 1 => new RelayArray1[E](head)
		case _ => outOfBounds_!(index, 2)
	}

	override def removed(from :Int, until :Int) :RelayArray[E] =
		if (until <= from | until <= 0 | from >= 2)
			this
		else if (from <= 0 && until >= 2)
			RelayArray.empty
		else if (from <= 0) //until == 1
			new RelayArray1[E](last)
		else //from == 1 && until >= 2
			new RelayArray1[E](head)

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
		newInstance(copy, 0, 3)
	}

	override def appended[U >: E](elem :U) :RelayArray[U] = {
		val copy = newArray[U](elem.getClass, InitSize)
		copy(2) = elem
		copyToArray(copy, 0, 2)
		newInstance(copy, 0, 3)
	}

	override def prepended[U >: E](elem :U) :RelayArray[U] = {
		val copy = newArray[U](elem.getClass, InitSize)
		val newOffset = InitSize - 3
		copy(newOffset) = elem
		copyToArray(copy, InitSize - 2, 2)
		newInstance(copy, newOffset, 3)
	}

	override def updatedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = elems match {
		case _ if index < 0 | index > 2               => outOfBounds_!(index, errorString(this) + ".updatedAll")
		case _ if elems.knownSize == 0                => this
		case _  :View[U]                              => updatedAll(index, elems.iterator)
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
				case Int.MinValue                    => this
				case _ if preferDropOverIterator(it) => overwritten(0, it.drop(-index))
				case _                               => overwritten(0, it.iterator.drop(-index))
			}
			case _ =>
				val it = elems.iterator
				if (!it.hasNext)
					this
				else index match {
					case Int.MinValue => this
					case 1            => RelayArray.two(head, it.next())
					case 0            =>
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
			case _ if index > 2 | index < 0     => outOfBounds_!(index, 3)
			case _ if size == 0                 => this
			case _  :View[U]                    => insertedAll(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case ar :RelayArray[U] if ar.elementType isConvertibleTo elementType =>
				val res = newArray(math.max(size + 2, InitSize))
				ar.copyToArray(res.asInstanceOf[Array[U]], index, Int.MaxValue)
				index match {
					case 0 => copyToArray(res, size, 2)
					case 1 => copyToArray(res, 0, 1); copyRangeToArray(res, size + 1, 1, 1)
					case 2 => copyToArray(res, 0, 2)
				}
				newSpecific(res, 0, size + 2, true)
			case _ =>
				val res = RelayArray.newBuilder[U]
				res.sizeHint(elems, 2)
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
					new RelayArrayPlus[U](copy, 0, newSize, true)
				}
			case it :Iterable[U] => it.knownSize match {
				case -1 => super.appendedAll(it)
				case  n =>
					try {
						val capacity = math.max(n + 2, InitSize)
						val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
						write(copy, 0, 0, 2) //May throw ClassCastException
						it.copyToArray(copy.asInstanceOf[Array[U]], 2)
						newSpecific(copy, 0, n + 2, true)
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							val capacity = math.max(n + 2, InitSize)
							val copy = new Array[AnyRef](capacity).asInstanceOf[Array[U]]
							copy(0) = head
							copy(1) = last
							it.copyToArray(copy, 2, n + 2)
							new RelayArrayPlus[U](copy, 0, n + 2, true)
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
			new RelayArrayPlus[E](copy, 0, newSize, true)
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
					val copy = new Array[Any](capacity).asInstanceOf[Array[U]]
					copy(capacity - 2) = head
					copy(capacity - 1) = last
					it.copyToArray(copy, capacity - newSize)
					new RelayArrayPlus[U](copy, capacity - newSize, newSize, true)
				}
			case it :Iterable[U] => it.knownSize match {
				case -1 => super.prependedAll(it)
				case  n =>
					try {
						val capacity = math.max(n + 2, InitSize)
						val copy = ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[E]]
						write(copy, 0, capacity - 2, 2)
						val newOffset = capacity - 2 - n
						it.copyToArray(copy.asInstanceOf[Array[U]], newOffset, n)
						newSpecific(copy, newOffset, newOffset + n + 2, true)
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
			new RelayArrayPlus[E](copy, 0, newSize, true)
		}


	override def copyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from >= 2 || start >= xs.length)
			0
		else if (start < 0)
			outOfBounds_!(
				errorString(this) + ".copyRangeToArray(" + errorString(xs) + ", " + start + ", " + from + ", " + len + ")"
			)
		else {
			val from0  = math.min(math.max(from, 0), 2)
			val copied = math.min(2 - from0, math.min(xs.length - start, len))
			def copy() :Unit = from0 match {
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
						write(xs.asInstanceOf[Array[E]], from0, start, copied)
					else
						copy()
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						copy()
				}
			copied
		}

	override def cyclicCopyRangeToArray[U >: E](xs :Array[U], start :Int, from :Int, len :Int) :Int =
		if (len <= 0 | from >= 2 || xs.length == 0)
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
						write(xs.asInstanceOf[Array[E]], from, start, copied) //May throw ClassCastException
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
  * and [[net.noresttherein.sugar.collections.RelayArrayRange RelayArrayRange]].
  */ //todo: rename to SubRelayArray or RelayArraySlice; problem with 'Slice' is slice method from Iterable
private sealed trait ProperRelayArray[@specialized(ElemTypes) +E]
	extends RelayArrayPrivates[E] with ApplyPreferredSeqOps[E, RelayArray, RelayArray[E]]
	   with ArraySliceSeqOps[E, RelayArray, RelayArray[E]]
{  //todo: review and try to use Array.copyOf rather than separate creation and copying of arrays.
	override def elementType :Class[_] = unsafeArray.getClass.getComponentType
	override def isImmutable :Boolean = true
	protected var isOwner    :Boolean
	protected def canPassOn  :Boolean

	private[collections] def dumpString :String =
		mkString(s"${this.localClassName}|$startIndex-${startIndex + length}/${ unsafeArray.length}|(", ", ", ")")

	protected final def maxCapacityError(extras :Int) =
		maxSize_!("Cannot add " + extras + " elements to " + errorString(this) +
			": maximum array size of " + MaxArraySize + " exceeded."
		)

	override def updated[U >: E](index :Int, elem :U) :RelayArray[U] = {
		val len = length
		if (index < 0 | index >= len)
			outOfBounds_!(index.toString + " out of " + len)
		val array = unsafeArray.asInstanceOf[Array[U]]
		val offset = startIndex
		val elemType = elementType
		if ((elemType.isPrimitive || elemType.isBox) && array(offset + index) == elem)
			this
		else if (elemType accepts elem.getClass) {
			val res = array.slice(offset, offset + length)
			res(index) = elem
			newInstance(res, 0, length)
		} else {
			val res = new Array[Any](length)
			Array.copy(array, offset, res, 0, length)
			res(index) = elem
			new RelayArrayPlus[U](res.asInstanceOf[Array[U]], 0, length)
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
			newInstance(res, 0, length)
		}
		elems match {
			case _ if index < 0 | index > length || size > length - index =>
				outOfBounds_!(errorString(this) + ".updatedAll(" + index + ", " + errorString(elems) + ")")
			case _ if size == 0                               => this
			case _  :View[U]                                  => updatedAll(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty                => this
			case it :Iterator[U] if !it.hasNext               => this
			case _ if size > 0 && elementType == classOf[Any] => overwriteWith(classOf[Any])
			case ar :RelayArray[U]                            => overwriteWith(ar.elementType)
			case ArrayLike.Slice(array, _, _)                 => overwriteWith(array.getClass.getComponentType)
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
					} catch { //We must catch NullPointerException in case we try to write null to a value type array.
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							res = ArrayFactory.copyAs[U](res, classOf[Any], length)
							res(i) = next
							while (it.hasNext) {
								i += 1
								res(i) = it.next()
							}
					}
					newInstance(res, 0, length)
				}
		}
	}

	override def overwritten[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		val size   = elems.knownSize
		val length = this.length
		val offset = math.max(index, 0)
		val drop   = math.max(0, -math.max(index, -Int.MaxValue))
		def overwriteWith(elemType :Class[_]) :RelayArray[U] = {
			val res =
				if (elemType.isConvertibleTo(elementType))
					ArrayFactory.copyOf(array, length).asInstanceOf[Array[U]]
				else
					ArrayFactory.copyAs[U](array, classOf[Any], length)
			elems.copyRangeToArray(res, offset, drop, Int.MaxValue)
			newInstance(res, 0, length)
		}
		elems match {
			case _ if index >= length | size == 0 | size > 0 & size <= drop => this
			case _  :View[U]                      => overwritten(index, elems.iterator)
			case it :Iterable[U] if it.isEmpty    => this
			case it :Iterator[U] if !it.hasNext   => this
			case _ if elementType == classOf[Any] => overwriteWith(classOf[Any])
			case ar :RelayArray[U]                => overwriteWith(ar.elementType)
			case ArrayLike.Slice(array, _, _)     => overwriteWith(array.getClass.getComponentType)
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
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							res = ArrayFactory.copyAs[U](res, classOf[Any], length)
							res(i) = next
							it.copyToArray(res, i + 1, Int.MaxValue)
					}
					newInstance(res, 0, length)
				}
		}
	}

	override def inserted[U >: E](index :Int, elem :U) :RelayArray[U] =
		if (index == 0) prepended(elem)
		else if (index == length) appended(elem)
		else if (index < 0 || index > length) outOfBounds_!(index, errorString(this) + ".inserted")
		else {
			val length = this.length
			if (length == MaxArraySize)
				maxCapacityError(1)
			val res = newArray[U](elem.getClass, length + 1)
			copyRangeToArray(res, 0, 0, index)
			res(index) = elem
			copyRangeToArray(res, index + 1, index, length - index)
			newInstance(res, 0, length + 1)
		}

	override def insertedAll[U >: E](index :Int, elems :IterableOnce[U]) :RelayArray[U] = {
		def insertAs(elemType :Class[_], elemsSize :Int) :RelayArray[U] = {
			if (elemsSize > MaxArraySize - length)
				maxCapacityError(elemsSize)
			val res =
				if (elemType.isConvertibleTo(elementType))
					newArray[U](elementType, length + elemsSize)
				else
					newArray[U](classOf[Any], length + elemsSize)
			copyRangeToArray(res, 0, 0, index)
			elems.toBasicOps.copyToArray(res, index, Int.MaxValue)
			copyRangeToArray(res, index + elemsSize, index, Int.MaxValue)
			newInstance(res, 0, length + elemsSize)
		}
		if (index == 0)
			prependedAll(elems)
		else if (index == length)
			appendedAll(elems)
		else if (index < 0 || index > length)
			outOfBounds_!(errorString(this) + ".insertedAll(" + index + ", " + errorString(elems) + ")")
		else if (elems.knownSize == 0)
			this
		else elems match {
			case other :RelayArray[U]                => insertAs(other.elementType, other.length)
			case ArrayLike.Slice(array, from, until) => insertAs(array.getClass.getComponentType, until - from)
			case _ =>
				val res = RelayArray.newBuilder[U]
				res.sizeHint(elems, length)
				res ++= range(0, index) ++= elems ++= range(index, length)
				res.result()
		}
	}


	override def range(from :Int, until :Int) :RelayArray[E] = {
		val offset = startIndex; val len = length
		if (until <= from | until <= 0 | from >= len)
			RelayArray.empty
		else if (from <= 0 && until >= len)
			new RelayArrayRange[E](array, offset, len)
		else {
			val array   = unsafeArray.asInstanceOf[Array[E]]
			val start   = math.max(from, 0)
			val newSize = math.min(len, until) - start
			if (newSize == 1)
				new RelayArray1[E](array(offset + start))
			else if (newSize == 2)
				new RelayArray2[E](array(offset + start), array(offset + start + 1), elementType)
			else
				new RelayArrayRange[E](array, offset + start, newSize)
		}
	}

	override def shrink :RelayArray[E] =
		if (unsafeArray.length == length)
			this
		else {
			val copy = unsafeArray.asInstanceOf[Array[E]].slice(startIndex, startIndex + length)
			new RelayArrayPlus[E](copy)
		}


	override def appended[U >: E](elem :U) :RelayArray[U] = {
		val len = length
		if (len == MaxArraySize)
			maxCapacityError(1)
		val arr      = array
		val offset   = startIndex
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		var res      = null :RelayArray[U]
		if (canStore) //todo: instead of verifying beforehand, catch a thrown exception.
			if (offset + len < arr.length) {
				if (canPassOn) {
					arr(offset + len) = elem.asInstanceOf[E]
					res = newSpecific(arr, offset, offset + len + 1, true)
				}
			} else if (len == 0 && arr.length > 0 && canPassOn) {
				arr(0) = elem.asInstanceOf[E]
				res = newSpecific(arr, 0, 1, true)
			}
		if (res == null) {
			val halfSize  = len + 1 >> 2
			val newOffset = math.max(0, math.min(math.min(offset, halfSize), MaxArraySize - len - halfSize))
			val newEnd    = newOffset + len + 1
			val capacity  = math.max(newEnd + math.min(MaxArraySize - newEnd, halfSize), InitSize)
			if (capacity > MaxArraySize)
				maxCapacityError(1)
			val copy = //todo: Use Array.copyOf (Array.copyOfRange, that is)
				if (canStore) ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[U]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[U]]
			copy(newOffset + len) = elem
			ArrayLike.copy(arr, offset, copy, newOffset, len)
			res =
				if (canStore) newSpecific(copy.asInstanceOf[Array[E]], newOffset, newOffset + len + 1, true)
				else new RelayArrayPlus[U](copy, newOffset, len + 1, true)
		}
		res
	}

	override def prepended[U >: E](elem :U) :RelayArray[U] = {
		val len = length
		if (len == MaxArraySize)
			maxCapacityError(1)
		val arr      = array
		val offset   = startIndex
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		var res      = null :RelayArray[U]
		if (canStore)
			if (offset > 0) {
				if (canPassOn) {
					arr(offset - 1) = elem.asInstanceOf[E]
					res = newSpecific(arr, offset - 1, offset + len, true)
				}
			} else if (len == 0 && arr.length > 0 && canPassOn) {
				val newOffset = arr.length - 1
				arr(newOffset) = elem.asInstanceOf[E]
				res = newSpecific(arr, newOffset, newOffset + 1, true)
			}
		if (res == null) {
			val halfSize  = len + 1 >> 2
			val padding   = math.min(arr.length - offset - len, halfSize)
			val capacity  = math.min(MaxArraySize, math.max(halfSize +~ len +~ padding, InitSize))
			val newOffset = math.max(1, capacity - padding - len)
			if (newOffset + len > MaxArraySize)
				maxCapacityError(1)
			val copy =
				if (canStore) ArrayFactory.ofDim(elementType, capacity).asInstanceOf[Array[U]]
				else new Array[AnyRef](capacity).asInstanceOf[Array[U]]
			ArrayLike.copy(arr, offset, copy, newOffset, len)
			copy(newOffset - 1) = elem
			res =
				if (canStore) newSpecific(copy.asInstanceOf[Array[E]], newOffset - 1, newOffset + len, true)
				else new RelayArrayPlus[U](copy, newOffset - 1, len + 1)
		}
		res
	}

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] = {
		val len      = length
		val arr      = array
		val offset   = startIndex
		val capacity = arr.length
		suffix match { //We don't want to evaluate elements of a view multiple times.
			case it :View[U]                    => appendedAll(it.iterator)
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] if len == 0 && (it.length > capacity || !isOwner) => it
			case it :RelayArray[U] =>
				val extras    = it.length
				val newSize   = len + extras
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				var res       = null :RelayArray[U]
				if (canStore)
					if (offset + newSize <= capacity) {
						if (canPassOn) {
							it.copyToArray(arr.asInstanceOf[Array[U]], offset + len, extras)
							res = newSpecific(arr, offset, offset + newSize, true)
						}
					} else if (len == 0 && extras <= capacity && canPassOn) {
						it.copyToArray(arr.asInstanceOf[Array[U]], 0, extras)
						res = newSpecific(arr, 0, extras, true)
					}
				if (res == null)
					res = appendedAll(it, if (canStore) myType else classOf[Any], extras)
				res
			case it :Iterable[U] => it.knownSize match {
				case -1 =>
					super.appendedAll(it)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :RelayArray[U] = null
						if (offset + newSize <= unsafeArray.length) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(arr.asInstanceOf[Array[U]], offset + len, newSize)
								res = newSpecific(arr, offset, offset + newSize, true)
							}
						}
						if (res == null)
							res = appendedAll(it, arr.getClass.getComponentType, extras)
						res
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							if (wasOwner)
								isOwner = true
							appendedAll(it, classOf[Any], extras)
					}
			}
			case _ => super.appendedAll(suffix)
		}
	}

	private def appendedAll[U >: E](items :Iterable[U], elemType :Class[_], itemsSize :Int) :RelayArray[U] = {
		val len = length
		if (itemsSize > MaxArraySize - len)
			maxCapacityError(itemsSize)
		val halfSize   = (len + 1) / 2
		val newSize    = len + itemsSize
		val growth     = math.max(itemsSize, halfSize)
		val offset     = startIndex
		val offsetIdea = math.min(offset, halfSize)
		val desiredCap = offsetIdea +~ len +~ growth
		val capacity   = math.min(math.max(desiredCap, InitSize), MaxArraySize)
		val newOffset  = math.max(0, math.min(offsetIdea, capacity - len - growth))
		//capacity - len - extras|growth does not overflow: 0 <= len <= capacity & 0 < growth|extras <= Int.MaxValue
		val copy = ArrayFactory.ofDim(elemType, capacity).asInstanceOf[Array[U]]
		items.copyToArray(copy, newOffset + len, Int.MaxValue)
		ArrayLike.copy(array, offset, copy, newOffset, len)
		if (elemType eq elementType)
			newSpecific(copy.asInstanceOf[Array[E]], newOffset, newOffset + newSize, true)
		else
			new RelayArrayPlus(copy, newOffset, newSize, true)
	}


	override def prependedAll[U >: E](prefix :IterableOnce[U]) :RelayArray[U] = {
		val len      = length
		val arr      = array
		val offset   = startIndex
		val capacity = arr.length
		prefix match { //We don't want to evaluate elements of a view multiple times.
			case it :View[U]                    => prependedAll(it.iterator)
			case it :Iterable[U] if it.isEmpty  => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] if len == 0 && (it.length > capacity || !isOwner) => it
			case it :RelayArray[U] =>
				val extras    = it.length
				val newSize   = len + extras
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				var res       = null :RelayArray[U]
				if (canStore)
					if (offset >= extras) {
						if (canPassOn) {
							it.copyToArray(arr.asInstanceOf[Array[U]], offset - extras, extras)
							res = newSpecific(arr, offset - extras, offset - extras + newSize, true)
						}
					} else if (len == 0 && extras <= capacity && canPassOn) {
						val newOffset = capacity - extras
						it.copyToArray(arr.asInstanceOf[Array[U]], newOffset, extras)
						res = newSpecific(arr, newOffset, newOffset + extras, true)
					}
				if (res == null)
					res = prependedAll(it, if (canStore) myType else classOf[Any], extras)
				res
			case it :Iterable[U] => it.knownSize match {
				case -1 => super.prependedAll(prefix)
				case extras =>
					val newSize = len + extras
					var wasOwner = false
					try {
						var res :RelayArray[U] = null
						if (offset >= extras) {
							wasOwner = canPassOn
							if (wasOwner) {
								it.copyToArray(arr.asInstanceOf[Array[U]], offset - extras, extras)
								res = newSpecific(arr, offset - extras, offset - extras + newSize, true)
							}
						}
						if (res == null)
							res = prependedAll(it, elementType, extras)
						res
					} catch {
						case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
							if (wasOwner)
								isOwner = true
							prependedAll(it, classOf[Any], extras)
					}
			}
			case _ => super.prependedAll(prefix)
		}
	}

	private def prependedAll[U >: E](items :Iterable[U], elemType :Class[_], itemsSize :Int) :RelayArray[U] = {
		val len = length
		if (itemsSize > MaxArraySize - len)
			maxCapacityError(itemsSize)
		val arr        = array
		val newSize    = len + itemsSize
		val halfSize   = (len + 1) / 2
		val offset     = startIndex
		val padding    = math.min(arr.length - offset - len, halfSize)
		val offsetIdea = math.max(0, halfSize - itemsSize)
		val desiredCap = offsetIdea +~ newSize +~ padding
		val capacity   = math.min(MaxArraySize, math.max(desiredCap, InitSize))
		val newOffset  = math.min(offsetIdea, capacity - newSize)
		val copy       = ArrayFactory.ofDim(elemType, capacity).asInstanceOf[Array[U]]
		items.copyToArray(copy, newOffset, itemsSize)
		ArrayLike.copy(arr, offset, copy, newOffset + itemsSize, len)
		if (elemType eq elementType)
			newSpecific(copy.asInstanceOf[Array[E]], newOffset, newOffset + newSize, true)
		else
			new RelayArrayPlus(copy, newOffset, newSize, true)
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

	protected[this] def writeReplace :AnyRef =
		new ArraySerializationProxy[IArrayLike, E](RelayArray, array.asInstanceOf[IArrayLike[E]], startIndex, length)
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
  * @see [[net.noresttherein.sugar.collections.RelayArrayRange RelayArrayRange]]
  */
@SerialVersionUID(Ver)
private final class RelayArrayPlus[@specialized(ElemTypes) +E] private[collections]
	                              (arr :Array[E], offset :Int, len :Int, owner :Boolean = false)
	extends AbstractSeq[E] with ProperRelayArray[E]
{
	private[collections] def this(array :Array[E]) = this(array, 0, array.length)

//	assert(offset >= 0, "negative offset " + offset + " out of " + arr.length)
//	assert(len >= 0, "negative length " + len)
//	assert(len <= arr.length - offset, "array[" + arr.length + "].slice(" + offset + ", " + (offset + len) + ")")

	protected override def array :Array[E @uncheckedVariance] = arr
	private[sugar] override def startIndex :Int = offset
	@volatile private[this] var owns = owner //This assignment works as a memory fence ensuring visibility of the array.
	protected override def isOwner :Boolean = owns
	protected override def isOwner_=(value :Boolean) :Unit = owns = value

	override def length :Int = len

	private def ownerField = MethodHandles.lookup().findVarHandle(
		classOf[RelayArrayPlus[Any]], "owns", classOf[Boolean]
//		classOf[RelayArrayPlus[Any]].getName.replace('.', '$') + "$$owns", classOf[Boolean] //if the field were accessed from specialized subclasses
	)

	protected override def canPassOn :Boolean = {
		var read :Boolean = owns
		while (read && !OwnerField.weakCompareAndSet(this, true, false))
			read = owns
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
				new RelayArray1[E](arr(offset + from))
			case 2 =>
				new RelayArray2[E](arr(offset + from), arr(offset + from + 1), elementType)
			case len if len < arr.length / SliceReallocationFactor =>
				val copy = Array.like(array, len)
				ArrayLike.copy(arr, offset + from, copy, 0, len)
				new RelayArrayPlus[E](copy)
			case len =>
				new RelayArrayPlus[E](arr, offset + from, len)
		}

	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :RelayArray[E] = {
		val len = until - from
		if (len < arr.length / SliceReallocationFactor)
			new RelayArrayPlus[E](array.slice(from, until), 0, len, false)
		else
			new RelayArrayPlus[E](array, from, len, array ne this.arr)
	}
}




/** An array backed implementation of [[net.noresttherein.sugar.collections.RelayArray PassedArray]]
  * which is not the owner of the underlying array (hence all appends and prepends will allocate a new array).
  * Additionally, slicing operations ''always'' return a view over the same array instance, never reallocating
  * the elements (unlike in [[net.noresttherein.sugar.collections.RelayArrayPlus PassedArrayPlus]].
  */
@SerialVersionUID(Ver)
private final class RelayArrayRange[@specialized(ElemTypes) +E] private[collections]
	                               (arr :Array[E], offset :Int, len :Int)
	extends AbstractSeq[E] with ProperRelayArray[E]
{
	releaseFence()
	protected override def array :Array[E @uncheckedVariance] = arr
	private[sugar] override def startIndex :Int = offset
	override def length :Int = len

	protected override def canPassOn :Boolean = false
	protected override def isOwner   :Boolean = false
	protected override def isOwner_=(value :Boolean) :Unit = unsupported_!("RelayArrayRange.isOwner = " + value)

	override def apply(i :Int) :E =
		if (i < 0 | i >= len)
			outOfBounds_!("RelayArray|" + len + "|(" + i + ")")
		else
			arr(offset + i)

	override def temp :RelayArray[E] = this
	override def downsize :RelayArray[E] = {
		val copy = ArrayFactory.ofDim(elementType, len).asInstanceOf[Array[E]]
		ArrayLike.copy(arr, offset, copy, 0, len)
		new RelayArrayPlus[E](arr, 0, len)
	}

	override def range(from :Int, until :Int) :RelayArray[E] =
		if (from <= 0 && until > len) this else super.range(from, until)

	override def slice(from :Int, until :Int) :RelayArray[E] = range(from, until)
	protected override def clippedSlice(from :Int, until :Int) :RelayArray[E] = range(from, until)
	protected[this] override def newSpecific(array :Array[E], from :Int, until :Int) :RelayArray[E] =
		if (array eq this.arr) new RelayArrayRange[E](array, from, until - from)
		else new RelayArrayPlus[E](array, from, until - from, true)


/*	//fixme: we don't check for MaxArraySize
	//fixme: overflows/underflows

	override def appended[U >: E](elem :U) :RelayArray[U] = {
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		if (canStore) {
			val copy = ArrayFactory.ofDim(elementType, len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			ArrayLike.copy(arr, offset, copy, 0, len)
			newSpecific(copy, 0, len + 1, true)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			copy(len) = elem.asInstanceOf[E]
			ArrayLike.copy(arr, offset, copy, 0, len)
			new RelayArrayPlus[E](copy, 0, len + 1)
		}
	}

	override def prepended[U >: E](elem :U) :RelayArray[U] = {
		val elemType = elem.getClass
		val canStore = elemType isConvertibleTo elementType
		if (canStore) {
			val copy = ArrayFactory.ofDim(elementType, len + 1).asInstanceOf[Array[E]]
			ArrayLike.copy(arr, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			newSpecific(copy, 0, len + 1, true)
		} else {
			val copy = new Array[AnyRef](len + 1).asInstanceOf[Array[E]]
			ArrayLike.copy(arr, offset, copy, 1, len)
			copy(0) = elem.asInstanceOf[E]
			new RelayArrayPlus[E](copy, 0, len + 1)
		}
	}

	override def appendedAll[U >: E](suffix :IterableOnce[U]) :RelayArray[U] =
		suffix match {
			case it :Iterable[U] if it.isEmpty => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] if len == 0 => it
			case it :RelayArray[U] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				if (canStore) {
					val copy = ArrayFactory.ofDim(myType, len + extras).asInstanceOf[Array[U]]
					ArrayLike.copy(arr, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					newSpecific(copy.asInstanceOf[Array[E]], 0, copy.length, true)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[U]]
					ArrayLike.copy(arr, offset, copy, 0, len)
					it.copyToArray(copy, len, extras)
					new RelayArrayPlus[U](copy, 0, copy.length)
				}
			case _ => super.appendedAll(suffix)
		}


	override def prependedAll[U >: E](prefix :IterableOnce[U]) :RelayArray[U] =
		prefix match {
			case it :Iterable[U] if it.isEmpty => this
			case it :Iterator[U] if !it.hasNext => this
			case it :RelayArray[U] if len == 0 => it
			case it :RelayArray[U] =>
				val extras    = it.length
				val myType    = elementType
				val theirType = it.elementType
				val canStore  = theirType isConvertibleTo myType
				if (canStore) {
					val copy = ArrayFactory.ofDim(elementType, len + extras).asInstanceOf[Array[U]]
					ArrayLike.copy(unsafeArray, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					newSpecific(copy.asInstanceOf[Array[E]], 0, len + extras, true)
				} else {
					val copy = new Array[AnyRef](len + extras).asInstanceOf[Array[U]]
					ArrayLike.copy(unsafeArray, offset, copy, extras, len)
					it.copyToArray(copy, 0, extras)
					new RelayArrayPlus[E](copy.asInstanceOf[Array[E]], 0, len + extras)
				}
			case _ => super.prependedAll(prefix)
		}
*/
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
  * @define Arr `IArray`
  * @define arr immutable array
  */
@SerialVersionUID(Ver)
case object RelayArray extends ArrayLikeSliceFactory[IArrayLike, RelayArray] {

	override def from[E](it :IterableOnce[E]) :RelayArray[E] = it match {
		case elems :RelayArray[E] =>
			elems
		case _ if it.knownSize == 0 =>
			Empty
		case elems :View[E] =>
			//Views delegate isEmpty to iterator.isEmpty, but are common arguments,
			// so there is no sense in creating the iterator twice.
			new RelayArrayPlus(elems.toArray(ClassTag.Any.castParam[E]))
		case elems :Iterable[E] if elems.isEmpty => //usually, it should be also knownSize == 0, but it's not guaranteed.
			Empty
		case elems :Iterable[E] if elems.sizeIs <= 2 =>
			if (elems.size == 1) one(elems.head) else two(elems.head, elems.last)
		case elems :ArraySeq[E] =>
			make(elems.unsafeArray.asInstanceOf[IArrayLike[E]], 0, elems.length)
		//todo: IArray extractor not requiring a ClassTag
//		case IArray.Wrapped.Slice(array, from, until) =>
//			make(array.castFrom[IArray[_], Array[E]], from, until - from)
		case IArrayLike.Slice(array, from, until) =>
			new RelayArrayPlus[E](array.castFrom[IArrayLike[_], Array[E]], from, until - from)
		case ErasedArray.Slice(array, from, until) =>
			make(array.slice(from, until).asInstanceOf[IArray[E]], 0, until - from)
		case elems :Iterable[E] => elems.head.getClass match {
			case Boxed(valueClass) =>
				try wrap(IArray.from(elems)(ClassTag[E](valueClass))) catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						new RelayArrayPlus(elems.toArray(ClassTag.Any.castParam[E]))
				}
			case _                    =>
				new RelayArrayPlus(elems.toArray(ClassTag.Any.castParam[E]))
		}
		case _ =>
			val i = it.iterator
			if (!i.hasNext) Empty
			else new RelayArrayPlus(i.toArray(ClassTag.Any.castParam[E]))
	}

	/** A `RelayArray` containing the elements of the given array. If the array has two or fewer elements,
	  * a dedicated instance is returned, not backed by an array. Otherwise, the array is copied and wrapped.
	  * This method will aggressively attempt to create a collection backed by an array of a value type,
	  * if the argument contains only instances of a box class for one of the built in value types.
	  */
	def from[E](array :ArrayLike[E]) :RelayArray[E] = array.length match {
		case 0 => Empty
		case 1 => one(array(0))
		case 2 => two(array(0), array(1))
		case n =>
			if (array.getClass == classOf[Array[Any]])
				array(0).getClass match {
					case Boxed(valCls) => wrap(
						try IArray.copyAs(array, valCls.castParam[E]) catch {
							case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
								IArrayLike.copyOf(array, n)
						}
					)
					case _ => wrap(IArrayLike.copyOf(array, n))
				}
			else
				wrap(IArrayLike.copyOf(array, n))
	}

	/** A `RelayArray` containing the elements from the specified range of given array.
	  * If the array has two or fewer elements, a dedicated instance is returned, not backed by an array.
	  * Otherwise, the returned instance will be backed by an array with the copied contents.
	  * This method will aggressively attempt to create a collection backed by an array of a value type,
	  * if the argument contains only instances of a box class for one of the built in value types.
	  */
	def from[E](array :ArrayLike[E], offset :Int, length :Int) :RelayArray[E] =
		if (offset < 0 || length < 0 || length > array.length - offset)
			outOfBounds_!("RelayArray.from(" + errorString(array) + ", " + offset + ", " + length + ")")
		else length match {
			case 0 => Empty
			case 1 => one(array(offset))
			case 2 => two(array(offset), array(offset + 1))
			case _ =>
				val specialized =
					if (array.getClass != classOf[Any])
						No
					else array(0).getClass match {
						case Boxed(valClss) =>
							try {
								val newArray = ArrayFactory.ofDim(valClss.castParam[E], length)
								ArrayLike.copy(array, offset, newArray, 0, length)
								Yes(newArray.asInstanceOf[IArrayLike[E]])
							} catch {
								case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException => No
							}
						case _ => No
					}
				specialized match {
					case Yes(array) => wrap(array)
					case _          => wrap(IArrayLike.copyOfRange(array, offset, offset + length))
				}
		}


	/** Returns a $Coll backed by an array of a specific type, containing exactly the specified elements.
	  * This is very similar to standard `apply(elems :E*)`, but, as scala creates a reference array to contain
	  * variable arguments, the latter will create an instance backed by a reference array.
	  * This method will always create the array based on the `ClassTag`, at the cost of slower copying,
	  * which requires unboxing.
	  */
	def of[E :ClassTag](elems :E*) :RelayArray[E] =
		if (elems.knownSize >= 0)
			wrap(IArray.from(elems))
		else
			(specificBuilder[E] ++= elems).result()

	/** A $Coll backed by an identical copy of this array, containing all its elements.
	  * @see [[net.noresttherein.sugar.collections.RelayArray.wrap wrap]]
	  * @see [[net.noresttherein.sugar.collections.RelayArray.from from]]
	  */
	def copy[E](array :ArrayLike[E]) :RelayArray[E] = wrap(IArrayLike.copyOf(array))

	/** A $Coll backed by an array of length `until - from` and the same element type as `array`,
	  * containing elements `array.slice(from, until)`.
	  * @see [[net.noresttherein.sugar.collections.RelayArray.slice slice]]
	  * @see [[net.noresttherein.sugar.collections.RelayArray.from from]]
	  */
	def copy[E](array :ArrayLike[E], from :Int, until :Int) :RelayArray[E] =
		wrap(IArrayLike.copyOfRange(array, from, until))


	protected override def make[E](array :IArrayLike[E], from :Int, until :Int) :RelayArray[E] =
		make(array, from, until, false)

	private def make[E](array :IArrayLike[E], from :Int, until :Int, owner :Boolean) :RelayArray[E] =
		((array :ArrayLike[_]) match {
			case arr :Array[AnyRef]       => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Int]          => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Long]         => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Double]       => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Byte]         => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Char]         => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Float]        => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Short]        => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[Boolean]      => new RelayArrayPlus(arr, from, until - from, owner)
			case arr :Array[_]            => new RelayArrayPlus(arr, from, until - from, owner)
		}).castParam[E]

	override def isImmutable = true


	override def empty[E] :RelayArray[E] = Empty

	/** An empty $Coll backed by the array of the specified length. Similarly to the capacity argument
	  * to [[scala.collection.mutable.ArrayBuffer ArrayBuffer]] and [[StringBuilder]], it allows to avoid
	  * repeated reallocations if the future size is known, at least approximately. Each initial append/prepend
	  * will create only a lightweight object, backed by the array created here, for as long as its capacity
	  * is not exceeded.
	  */
	def empty[E](capacity :Int) :RelayArray[E] =
		if (capacity == 0) Empty
		else new RelayArrayPlus(new Array[Any](capacity).castFrom[Array[Any], Array[E]], 0, 0, true)

	/** An empty $Coll with an array of the specified element type. Appending to this instance
	  * will preserve the array type, as long as new elements can be stored (possibly after boxing/unboxing)
	  * in such an array.
	  */
	def empty[E](elementType :Class[E]) :RelayArray[E] = empty(elementType, InitSize)

	/** An empty $Coll backed by the array of the specified length and element type. Similarly to the capacity argument
	  * to [[scala.collection.mutable.ArrayBuffer ArrayBuffer]] and [[StringBuilder]], it allows to avoid
	  * repeated reallocations if the future size is known, at least approximately. Each initial append/prepend
	  * will create only a lightweight object, backed by the array created here, for as long as its capacity
	  * is not exceeded.
	  */
	def empty[E](elementType :Class[E], capacity :Int) :RelayArray[E] =
		make(ArrayFactory.ofDim(elementType, capacity).asInstanceOf[IArray[E]], 0, 0, true)

	/** An empty $Coll backed by an array of a specific type, based on an implicit class tag. */
	def emptyOf[E :ClassTag] :RelayArray[E] = emptyOf[E](InitSize)

	/** An empty $Coll backed by the array of the specified length. Similarly to the capacity argument
	  * to [[scala.collection.mutable.ArrayBuffer ArrayBuffer]] and [[StringBuilder]], it allows to avoid
	  * repeated reallocations if the future size is known, at least approximately. Each initial append/prepend
	  * will create only a lightweight object, backed by the array created here, for as long as its capacity
	  * is not exceeded.
	  * @return a $Coll backed by an `Array[E]` of length `capacity`.
	  */
	def emptyOf[E :ClassTag](capacity :Int) :RelayArray[E] =
		make(new Array[E](capacity).asInstanceOf[IArray[E]], 0, 0, true)


	/** Equivalent to [[net.noresttherein.sugar.collections.RelayArray.one one]]`(elem)` */
	@inline def :+[E](elem :E) :RelayArray[E] = one(elem)

	/** Equivalent to [[net.noresttherein.sugar.collections.RelayArray.from from]]`(elems)`.
	  * Use of this method is more convenient when the resulting $coll is being immediately appended to,
	  * as `from` has lower precedence than `:++`:
	  * {{{
	  *     RelayArray from seq1 :++ seq2  //error, parsed as RelayArray.from(seq1 :++ seq2)
	  *     RelayArray.from(seq1) :++ seq2 //OK, but more syntax noise
	  *     RelayArray :++ seq1 :++ seq2   //Correct, most legible
	  * }}}
	  */
	@inline def :++[E](elems :IterableOnce[E]) :RelayArray[E] = from(elems)

	/** A specialized $Coll singleton. */
	def single[@specialized(ElemTypes) E](elem :E) :RelayArray[E] = new RelayArray1(elem)

	/** A $Coll singleton. This method is not `@specialized`, but produces an instance specialized
	  * for the runtime type of unboxed `elem`.
	  */
	def one[E](elem :E) :RelayArray[E] = elem match {
		case _ :Constable => (elem match {
			case x :Int     => new RelayArray1(x)
			case x :Long    => new RelayArray1(x)
			case x :Double  => new RelayArray1(x)
			case x :Char    => new RelayArray1(x)
			case x :Byte    => new RelayArray1(x)
			case x :Float   => new RelayArray1(x)
			case x :Short   => new RelayArray1(x)
			case x :Boolean => new RelayArray1(x)
			case _          => new RelayArray1(elem)
		}).asInstanceOf[RelayArray[E]]
		case _ => new RelayArray1(elem)
	}

	/** A special $Coll of only two elements. */
	def two[A](first :A, second :A) :RelayArray[A] = {
		val firstClass = first.getClass.unwrapped
		val secondClass = second.getClass.unwrapped
		if (firstClass != secondClass)
			new RelayArray2(first, second)
		else //only for as long as it is specialized
			(if (classOf[AnyRef].isAssignableFrom(firstClass))
				new RelayArray2(first, second, classOf[AnyRef])
			else if (firstClass == classOf[Int])
				new RelayArray2[Int](first.asInstanceOf[Int], second.asInstanceOf[Int], classOf[Int])
			else if (firstClass == classOf[Long])
				new RelayArray2[Long](first.asInstanceOf[Long], second.asInstanceOf[Long], classOf[Long])
			else if (firstClass == classOf[Double])
				new RelayArray2[Double](first.asInstanceOf[Double], second.asInstanceOf[Double], classOf[Double])
			else if (firstClass == classOf[Byte])
				new RelayArray2[Byte](first.asInstanceOf[Byte], second.asInstanceOf[Byte], classOf[Byte])
			else if (firstClass == classOf[Char])
				new RelayArray2[Char](first.asInstanceOf[Char], second.asInstanceOf[Char], classOf[Char])
			else if (firstClass == classOf[Float])
				new RelayArray2[Float](first.asInstanceOf[Float], second.asInstanceOf[Float], classOf[Float])
			else if (firstClass == classOf[Short])
				new RelayArray2[Short](first.asInstanceOf[Short], second.asInstanceOf[Short], classOf[Short])
			else if (firstClass == classOf[Boolean])
				new RelayArray2[Boolean](first.asInstanceOf[Boolean], second.asInstanceOf[Boolean], classOf[Boolean])
			else
				new RelayArray2(first, second, classOf[Any])
			).castParam[A]
	}



	/** An optimistic `RelayArray` builder; if the first element added is a value type (or, rather, a Java box
	  * for a Java primitive type), it allocates an array of the appropriate value type. The classes of subsequently
	  * added elements are checked until a first element with a type not fitting in the array is found,
	  * in which case the underlying array is reallocated as `Array[AnyRef]`.
	  * It is somewhat slower than [[net.noresttherein.sugar.collections.RelayArray.genericBuilder genericBuilder]],
	  * especially when adding individual elements, but considerably faster when adding `Array`-backed collections
	  * of the compatible element type: [[collection.immutable.ArraySeq immutable.ArraySeq]],
	  * [[collection.mutable.ArraySeq mutable.ArraySeq]] and $Coll itself.
	  */
	override def newBuilder[E] :Builder[E, RelayArray[E]] = new OptimisticBuilder[E](null)

	/** A builder of a `RelayArray` backed by an `Array[AnyRef]`. More efficient for reference types than
	  * [[net.noresttherein.sugar.collections.RelayArray.newBuilder newBuilder]].
	  */
	def genericBuilder[E] :Builder[E, RelayArray[E]] =
		newBuilder(classOf[AnyRef].castParam[E])

	/** A builder for a $Coll backed by an array with a specific element type, defined by an implicit [[ClassTag]]. */
	def specificBuilder[E :ClassTag] :Builder[E, RelayArray[E]] =
		newBuilder(classTag[E].runtimeClass.asInstanceOf[Class[E]])

	/** A builder for a $Coll backed by an array with the specified element type. */
	def newBuilder[E](elementType :Class[E]) :Builder[E, RelayArray[E]] =
		new SpecificBuilder(elementType)


	/** A [[scala.collection.ClassTagSeqFactory ClassTagSeqFactory]] creating
	  * [[net.noresttherein.sugar.collections.RelayArray RelayArray]]s backed by arrays of specific element types
	  * (defined by a `ClassTag` type class).
	  * @define Coll `RelayArray`
	  * @define coll relay array
	  */
	@SerialVersionUID(Ver)
	object specific extends ClassTagArrayLikeSliceSeqFactory[IArray, RelayArray] {
		protected override def make[E](array :IArray[E], from :Int, until :Int) :RelayArray[E] =
			RelayArray.make(array, from, until, false)

		override def from[E :ClassTag](it :IterableOnce[E]) :RelayArray[E] = it match {
			case relay :RelayArray[E] if relay.elementType == classTag[E].runtimeClass => relay
			case empty if empty.knownSize == 0                                         => this.empty[E]
			case IArray.Slice(array, from, until) if array.getClass.getComponentType == classTag[E].runtimeClass =>
				RelayArray.make(array, from, until, false)
			case _ => wrap(IArray.from(it))
		}

		override def empty[E :ClassTag] :RelayArray[E] = classTag[E].runtimeClass match {
			case ref if classOf[AnyRef].isAssignableFrom(ref) => wrap(IArray.empty[E])
			case classes.Int     => emptyIntRelay.castParam[E]
			case classes.Long    => emptyLongRelay.castParam[E]
			case classes.Double  => emptyDoubleRelay.castParam[E]
			case classes.Char    => emptyCharRelay.castParam[E]
			case classes.Byte    => emptyByteRelay.castParam[E]
			case classes.Float   => emptyFloatRelay.castParam[E]
			case classes.Short   => emptyShortRelay.castParam[E]
			case classes.Boolean => emptyBooleanRelay.castParam[E]
			case _               => wrap(IArray.empty[E])
		}

		private[this] val emptyByteRelay    = wrap(IArray.emptyByteIArray)
		private[this] val emptyCharRelay    = wrap(IArray.emptyCharIArray)
		private[this] val emptyShortRelay   = wrap(IArray.emptyShortIArray)
		private[this] val emptyIntRelay     = wrap(IArray.emptyIntIArray)
		private[this] val emptyLongRelay    = wrap(IArray.emptyLongIArray)
		private[this] val emptyDoubleRelay  = wrap(IArray.emptyDoubleIArray)
		private[this] val emptyFloatRelay   = wrap(IArray.emptyFloatIArray)
		private[this] val emptyBooleanRelay = wrap(IArray.emptyBooleanIArray)

		override def newBuilder[E :ClassTag] :Builder[E, RelayArray[E]] =
			new SpecificBuilder(classTag[E].runtimeClass.castParam[E])

		override def toString :String = "RelayArray.specific"
	}


	/** A [[scala.collection.SeqFactory SeqFactory]] building
	  * [[net.noresttherein.sugar.collections.RelayArray RelayArray]]s always backed by `Array[Any]`.
	  */
	@SerialVersionUID(Ver)
	object untagged extends ArrayLikeSliceFactory[IRefArray, RelayArray] {
		override def from[A](source :IterableOnce[A]) :RelayArray[A] = source match {
			case relay :RelayArray[A] if relay.elementType == classOf[Any] => relay
			case IRefArray.Slice(array, from, until)                       => make(array, from, until)
			case _                                                         => wrap(IRefArray.from(source))
		}
		override def empty[A] :RelayArray[A] = Empty

		override def newBuilder[A] :Builder[A, RelayArray[A]] = genericBuilder

		protected override def make[E](array :IRefArray[E], from :Int, until :Int) :RelayArray[E] =
			RelayArray.make(array, from, until, false)

		private[this] val Empty = wrap(IRefArray.empty[Nothing])

		override def toString = "RelayArray.untagged"
	}



	/** A builder of $Coll backed by an array of `elemType` component type. */
	private class SpecificBuilder[E](elemType :Class[E])
		extends ReusableBuilder[E, RelayArray[E]]
	{
		/** Array element type. Overridden in `OptimisticBuilder`. */
		protected def elementType         = elemType
		private[this] var elems :Array[E] = _
		private[this] var size            = 0
		private[this] var initSize        = InitSize
		override def knownSize            = size
		protected def knownSize_=(size :Int) :Unit = this.size = size

		def capacity :Int = if (elems == null) 0 else elems.length

		private def newArray(size :Int) :Array[E] =
			if (elementType == null)
				ArrayFactory.ofDim(classOf[Any].castParam[E], size)
			else
				ArrayFactory.ofDim(elementType, size)

		protected final def realloc(newSize :Int) :Unit =
			elems = ArrayFactory.copyOf(elems, elementType, newSize)

		override def sizeHint(size :Int) :Unit =
			if (size > 0 & size <= MaxArraySize)
				if (elementType == null)
					initSize = size
				else if (elems == null)
					elems = newArray(size)
				else if (size > elems.length)
					realloc(size)

		final def ensure(extras :Int) :Unit = {
			if (extras > MaxArraySize - size)
				maxSize_!(
					"Cannot add " + extras + " new elements to a Builder[" + elementType.localName +
						", RelayArray] with " + size + " elements: arithmetic overflow."
				)
			if (elems == null)
				elems = newArray(math.max(extras, initSize))
			else {
				val capacity = elems.length
				val newSize = size + extras
				if (newSize > capacity) {
					val newCapacity = math.max(math.min(MaxArraySize >> 1, capacity) << 1, newSize)
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
					val copied = it.copyToArray(elems, size)
					if (copied != n)
						illegalState_!(
							"RelayArray.newBuilder.addAll: " + errorString(xs) + " copied " + copied +
								" elements instead of " + n + "."
						)
					size += n
					this
			}
			case _ => super.addAll(xs)
		}
		override def addOne(elem :E) = {
			if (elems == null)
				elems = newArray(initSize)
			else if (size == elems.length) {
				if (size == Int.MaxValue)
					ensure(1) //throw a BufferFullException
				elems = Array.copyOf(elems, math.min(MaxArraySize >> 1, size) << 1)
			}
			elems(size) = elem
			size += 1 //Must be after write attempt in case the latter throws an exception.
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
			case 2 => new RelayArray2(elems(0), elems(1), elementType)
			case n if n < elems.length * AcceptableBuilderFillRatio =>
				val res = wrap(IArray.copyOf(elems, n))
				clear()
				res
			case _ =>
				val res = make(elems.asInstanceOf[IArrayLike[E]], 0, size)
				clear()
				res
		}
		override def toString = this.className + "[" + elementType.name + "]|" + size + "|"
	}


	/** A $Coll builder which starts with an array either of the specified element type (if not null),
	  * or the unboxed type of the first added element. If any future elements have an incompatible type,
	  * it reallocates the array as `Array[Any]` instead.
	  */
	private final class OptimisticBuilder[E](private[this] var elemType :Class[E])
		extends SpecificBuilder[E](elemType) with Builder[E, RelayArray[E]] //for access to super.addAll
	{
		private[this] var optimistic = elemType == null || elementType != classOf[Any]
		//overridden because we use it much more, so want to use a field and not a method call.
		override def elementType = elemType
//		def elementType_=(tpe :Class[E]) :Unit = elemType = tpe

		override def addOne(elem :E) :this.type = {
			if (elemType == null) {
				if (elem == null)
					elemType = classOf[Any].castParam[E]
				else {
					elemType = elem.getClass.unwrapped.castParam[E]
					if (!elemType.isPrimitive)
						elemType = classOf[Any].castParam[E]
				}
				super.addOne(elem)
			} else if (optimistic)
				try super.addOne(elem) catch {
					case _ :ArrayStoreException | _ :NullPointerException | _ :ClassCastException =>
						optimistic = false
						elemType = classOf[Any].castParam[E]
						realloc(math.max(capacity, InitSize))
						super.addOne(elem)
				}
			else
				super.addOne(elem)
		}
		override def addAll(xs :IterableOnce[E]) =
			xs match {
				case _ if !optimistic                     => super.addAll(xs)
				case empty :Iterable[_] if empty.isEmpty  => this
				case empty :Iterator[_] if !empty.hasNext => this
				case items :ArrayIterableOnce[E] => addKnownType(items.unsafeArray.getClass.getComponentType, items)
				case items :ArraySeq[E]          => addKnownType(items.unsafeArray.getClass.getComponentType, items)
				case items :mutable.ArraySeq[E]  => addKnownType(items.array.getClass.getComponentType, items)
				case items :Iterable[E]          => addOptimistic(items)
				case _                           => super[Builder].addAll(xs)
			}

		private def addKnownType(elemType :Class[_], xs :IterableOnce[E]) :this.type = {
			updateElementType(elemType, xs)
			super.addAll(xs)
		}
		//Requires xs.nonEmpty
		private def addOptimistic(xs :Iterable[E]) :this.type = {
			updateElementType(xs.head.getClass, xs)
			if (xs.knownPure) { //Use of bulk copyToArray is safe, because we can restart from the beginning again.
				val rollbackSize = knownSize
				try super.addAll(xs) catch { //Use xs.copyToArray
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						optimistic = false
						elemType = classOf[Any].castParam[E]
						knownSize = rollbackSize
						realloc(capacity)
						super.addAll(xs)
				}
			} else { //Add elements one by one, never reading the same element from xs twice.
				val i    = xs.iterator
				var next = i.next()
				try {
					addOne(next)
					while (i.hasNext) {
						next = i.next()
						addOne(next)
					}
				} catch {
					case _ :ClassCastException | _ :ArrayStoreException | _ :NullPointerException =>
						optimistic   = false
						elemType = classOf[Any].castParam[E]
						realloc(capacity)
						addOne(next)
						while (i.hasNext)
							addOne(i.next())
				}
				this
			}
		}

		private def updateElementType(newElemType :Class[_], xs :IterableOnce[_]) :Unit =
			if (elemType == null)
				elemType = (if (newElemType.unwrapped.isPrimitive) newElemType else classOf[Any]).castParam[E]
			else if (!newElemType.isConvertibleTo(elemType)) {
				optimistic = false
				val size    = knownSize
				val extras  = xs.knownSize
				val newSize =
					if (extras >= 0) math.max(math.min(size + extras, size * 2), InitSize)
					else if (size == capacity) math.max(size * 2, InitSize)
					else size
				elemType = classOf[Any].castParam[E]
				realloc(newSize)
			}
	}


	private[this] final val Empty = new RelayArray0

	override def toString = "RelayArray"
}


/** Internal methods and constants which must be public because they are used by multiple `RelayArray` subclasses,
  * and thus can't be in `RelayArray` object.
  */
private object RelayArrayPlus { //Must be RelayArrayPlus to access var handle for the owner field in RelayArrayPlus.

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

	val OwnerField = new RelayArrayPlus(Array.emptyObjectArray, 0, 0).ownerField
//	val OwnerField = MethodHandles.lookup().findVarHandle(
//		classOf[RelayArrayPlus[Any]],
//			"owns", classOf[Boolean]
//		classOf[RelayArrayPlus[Any]].getName.replace('.', '$') + "$$owns", classOf[Boolean]
//	)
}
