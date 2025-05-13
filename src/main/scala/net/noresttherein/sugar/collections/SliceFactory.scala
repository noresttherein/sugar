package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.StrictOptimizedSeqFactory

import net.noresttherein.sugar.??!
import net.noresttherein.sugar.typist.kinds.Pow




/** A factory wrapping containers of kind `S` in collections `C`.
  * This is a limited implementation, which supports only views over entire containers,
  * such as standard `ArraySeq`. See [[net.noresttherein.sugar.collections.ArrayLikeSliceWrapper ArrayLikeSliceWrapper]]
  * for a factory of collections backed by slices of larger arrays.
  * @see [[net.noresttherein.sugar.collections.ArraySeqFactory ArraySeqFactory]]
  * @define Coll collection
  * @define coll collection
  */
//Implementations of this trait are loaded dynamically by name, so the upper bound of ArrayLike is important
// as a modicum of type safety, although, ideally, we'd use IArrayLike specifically for that.
trait WrapperFactory[-S[_], +C[_]] extends Serializable {
	/** Wraps the given $Source in a $Coll. The collection will share the contents with `source`,
	  * and thus any modifications to either will be visible in the other.
	  */
	def wrap[E](source :S[E]) :C[E]

	//consider: we could instead have separate IArrayLikeWrapper and MutableArrayWrapper to enforce this relationship,
	// but it would involve tripling every descending trait.
	def isImmutable :Boolean = false
	def isMutable   :Boolean = false
}


/** A factory of `C` sequences backed by containers of kind `S`.
  * Combines the wrapping interface with standard `SeqFactory` for a single type;
  * used to define default array-backed sequences used by the library.
  * @see [[net.noresttherein.sugar.collections.DefaultArraySeq]]
  * @see [[net.noresttherein.sugar.collections.ArraySeqFactory.untagged]]
  */
trait SeqWrapperFactory[-S[E], +C[E] <: collection.SeqOps[E, collection.Seq, collection.Seq[E]]]
	extends StrictOptimizedSeqFactory[C] with WrapperFactory[S, C]




/** A basic factory of collections `C[A]` representing a slice of another container `S[A]`.
  * @define Coll collection
  * @define coll collection
  * @define Source container
  * @define source container
  */
trait SliceFactory[-S[_], +C[_]] extends Serializable {
	/** Creates a $coll over a slice of $source, exposing only elements `source(from), ..., source(until - 1)`.
	  * The $coll will share the contents with `source`, and thus any modifications to either will be visible
	  * in the other. If any of indices in the `[from, until)` range are negative or greater than the number of elements
	  * in `source`, they are ignored.
	  */ //consider: renaming to range
	def slice[E](source :S[E], from :Int, until :Int) :C[E]
}


/** $factoryInfo
  *
  * Implementations typically do not copy the elements of passed input $source, but have view semantics instead.
  * @note Some implementations may return elements in a different order. Polymorphic treatment of abstract values
  *       is inadvisable due to radically differing semantics.
  * @tparam S The kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  * @see [[net.noresttherein.sugar.collections.IndexedIterator]]
  * @define factoryInfo A factory creating ${coll}s over slices of ${source}s.
  *                     The ${coll}s rely on the random indexing of the underlying ${source}s.
  */
trait ExpandedSliceFactory[-S[_], +C[_]] extends SliceFactory[S, C] {
	/** Length of the argument $source used to determine the upper index bound. */
	protected def totalSizeOf[T](source :S[T]) :Int

	/** Build a $coll with elements from range `[from, until)` in the argument $source.
	  * All public factory methods delegate to this method. It can assume the arguments are valid.
	  */
	protected def make[T](source :S[T], from :Int, until :Int) :C[T]

	/** A $coll with the entirety of the argument $source, starting from the beginning. */
	def apply[T](source :S[T]) :C[T] =
		make(source, 0, totalSizeOf(source))

	/** A $coll with elements `source(first), source(first + 1), ..., source(source.length - 1)`.
	  * Negative first index is treated like zero, and indices greater than `source.size` result in an empty $coll.
	  */
	def from[T](source :S[T], first :Int) :C[T] = apply(source, first, Int.MaxValue)

	/** A $coll with elements of `source`, starting with index `first`, and continuing until the end
	  * of the $source or until `length` elements are returned - whatever comes sooner.
	  * Negative `length` is equivalent to zero.
	  */
	def apply[T](source :S[T], first :Int, length :Int) :C[T] = {
		val len   = totalSizeOf(source)
		val from  = math.max(0, math.min(len, first))
		val until = from + math.min(len - from, math.max(length, 0))
		make(source, from, until)
	}

	/** A $coll with elements `source(from), source(from + 1), ..., source(until - 1)` of the given container.
	  * If any of indices in the `[from, until)` range are negative or greater than the size of `source`,
	  * they are ignored.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (from >= len)                    make(source, len, len)
		else if (until <= 0)                make(source, 0, 0)
		else if (from <= 0 && until >= len) make(source, 0, len)
		else if (from <= 0)                 make(source, 0, until)
		else if (until >= len)              make(source, from, len)
		else if (until <= from)             make(source, from, from)
		else                                make(source, from, until)
	}
}




/** A factory of slices of some larger containers. Semantically equivalent
  * to the regular [[net.noresttherein.sugar.collections.ExpandedSliceFactory ExpandedSliceFactory]],
  * but passing the first index and slice length rather than an index range as the arguments to the collection/iterator.
  *
  * This is an implementation trait for collections/iterators which store their size as a field,
  * such as a [[net.noresttherein.sugar.collections.CountdownIterator CountdownIterator]].
  * @tparam S The type of container over which the collections produced by this factory iterate.
  * @tparam C The type of collections/iterators produced by this factory.
  */
trait SizedSliceFactory[-S[_], +C[_]] extends ExpandedSliceFactory[S, C] {
	protected override def make[T](source :S[T], first :Int, size :Int) :C[T]

	override def from[T](source :S[T], first :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (first >= len) make(source, len, 0)
		else if (first <= 0) make(source, 0, len)
		else make(source, first, len - first)
	}

	override def apply[T](source :S[T], first :Int, length :Int) :C[T] = {
		val len   = totalSizeOf(source)
		val from  = math.max(0, math.min(len, first))
		val size  = math.min(len - from, math.max(length, 0))
		make(source, from, size)
	}

	override def slice[T](source :S[T], from :Int, until :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (from >= len)                    make(source, len, 0)
		else if (until <= 0)                make(source, 0, 0)
		else if (from <= 0 && until >= len) make(source, 0, len)
		else if (from <= 0)                 make(source, 0, until)
		else if (until >= len)              make(source, from, len - from)
		else if (until <= from)             make(source, from, 0)
		else                                make(source, from, until - from)
	}
}




/** $factoryInfo
  *
  * The slices produced by this factory may not represent a consecutive fragment of the input $source,
  * but instead wrap at the end of the input container.
  * Index arguments to `slice` and `apply` are treated modulo the size of the $source.
  * @note Some implementations return elements in their reverse order,
  *       wrapping at the beginning of the input $source instead.
  * @tparam S The kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  * @see [[net.noresttherein.sugar.collections.ReverseCyclicSliceFactory]]
  * @define coll cyclic slice
  */ //consider: allowing to iterate multiple times over the array, treating the index modulo.
trait CyclicSliceFactory[-S[_], +C[_]] extends SizedSliceFactory[S, C] {
	/** A $coll with all elements of `source` in ascending index order,
	  * starting with index `first % source.length`. The index is increased modulo the size of `source`,
	  * wrapping back to the beginning when its end is reached.
	  */
	override def from[T](source :S[T], first :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (len <= 1)       make(source, 0, len)
		//This case would also cover the next one if it weren't for overflows.
		else if (first < 0) make(source, (len + first % len) % len, len)
		else                make(source, first % len, len)
	}

	/** A $coll with `length` elements of the $source, starting with `source(offset)`. If `offset + length`
	  * is greater than the size of the $source, then the $coll wraps to the beginning of the $source,
	  * returning `source(0)` following `source(source.size - 1)`, and so on, until `min(length, source.size)`
	  * elements are returned. If `length` is negative, the $coll will have no elements.
	  * @param source the backing $source.
	  * @param offset the index of the first element in the returned $Coll, modulo the size of `source`.
	  * @param length the maximum number of returned elements.
	  */ //consider: allowing returning more than source.length elements.
	override def apply[T](source :S[T], offset :Int, length :Int) :C[T] = {
		val len = totalSizeOf(source)
		val from =
			if (len <= 1) 0
			else if (offset < 0) (len + offset % len) % len
			else offset % len
		make(source, from, math.min(math.max(length, 0), len))
	}

	/** A $coll with subsequent elements of a $source, starting with index `from % source.size`,
	  * and increasing modulo the size of the $source until index `until % source.size` is reached (exclusive).
	  * If `from % source.size == until % source.size`, then the $coll will return the whole $source,
	  * unless also `from == until`, in which case it will be empty.
	  * @param source the $source with the elements to iterate.
	  * @param from   the index of the first returned element, modulo the length of the $source.
	  * @param until  the index immediately following the last element of the returned $coll.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (len == 0) //avoid division by zero
			make(source, 0, 0)
		else {
			val from0  = if (from < 0) (len + from % len) % len else from % len
			val until0 = if (until < 0) (len + until % len) % len else until % len
			val size   =
				if (until0 > from0) until0 - from0
				else if (from0 > until0) len + until0 - from0
				else if (from == until) 0
				else len
			make(source, from0, size)
		}
	}
}




/** A factory of collections/iterators over $source exposing the elements in the reverse order.
  * @tparam S The kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  * @define coll reverse slice
  */
trait ReverseSliceFactory[-S[_], +C[_]] extends ExpandedSliceFactory[S, C] {

	/** A $coll containing all elements of $source in their reverse order. */
	override def apply[T](source :S[T]) :C[T] = make(source, 0, totalSizeOf(source))

	/** A $coll with elements `source(first), source(first - 1), ..., source(0)`.
	  * If `first >= source.size`, then the whole $source is returned; if `first < 0`, then the returned $coll is empty.
	  */
	override def from[T](source :S[T], first :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (first <= -1) make(source, 0, 0)
		else if (first >= len) make(source, 0, len)
		else make(source, 0, first + 1)
	}

	/** A $coll with elements `source(first), source(first - 1), ..., source(first - length + 1)`.
	  * If `first >= source.size`, then `first` becomes the last element of the $source.
	  * Indexes lesser than zero or greater than the size of `source` are ignored.
	  */
	override def apply[T](source :S[T], first :Int, length :Int) :C[T] = {
		val len    = totalSizeOf(source)
		val from   = math.max(-1, math.min(len - 1, first))
		val downTo = from - math.min(from + 1, math.max(length, 0))
		make(source, downTo + 1, from + 1)
	}

	/** A $coll with elements `source(until - 1), source(until - 2), ..., source(from)`.
	  * If any of the indices in the `[from, until)` range is out of bounds for `source`, it is ignored.
	  * Note that the first index argument is the last element of the $coll, and the actual first element
	  * of the returned $coll resides at an index preceding the second argument.
	  */ //override for docs.
	override def slice[T](source :S[T], from :Int, until :Int) :C[T]
}


/** An implementation trait of [[net.noresttherein.sugar.collections.ExpandedSliceFactory ExpandedSliceFactory]]
  * producing collections/iterators exposing the elements of the input $source in their reverse order,
  * but with implementations based around the offset of the first element and the number of elements
  * in the returned slice, rather than an index range,
  * such as [[net.noresttherein.sugar.collections.ReverseCountdownIterator ReverseCountdownIterator]].
  * @tparam S The kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  */
trait ReverseSizedSliceFactory[-S[_], +C[_]] extends ReverseSliceFactory[S, C] {
	/** Changes the semantics of super [[net.noresttherein.sugar.collections.ReverseSliceFactory.make make]]
	  * by treating the last argument as the size for the created $coll, rather than the end index.
	  */
	protected override def make[T](source :S[T], first :Int, size :Int) :C[T]

	override def apply[T](source :S[T]) :C[T] = {
		val len = totalSizeOf(source)
		make(source, len - 1, len)
	}

	override def from[T](source :S[T], first :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (first <= -1)       make(source, -1, 0)
		else if (first >= len) make(source, len - 1, len)
		else                   make(source, first, first + 1)
	}

	override def apply[T](source :S[T], first :Int, length :Int) :C[T] = {
		val len  = totalSizeOf(source)
		val from = math.max(-1, math.min(len - 1, first))
		val size = math.min(from + 1, math.max(length, 0))
		make(source, from, size)
	}

	override def slice[T](source :S[T], from :Int, until :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (from >= len) make(source, len, 0)
		else if (until <= 0) make(source, -1, 0)
		else if (from <= 0 & until >= len) make(source, len - 1, len)
		else if (from <= 0) make(source, until - 1, until)
		else if (until >= len) make(source, len - 1, len - from)
		else if (until <= from) make(source, from, 0)
		else make(source, until - 1, until - from)
	}
}


/** $factoryInfo
  * The index arguments to `slice` and `apply` methods of this factory are treated modulo the size of the $source.
  * Created slices do not necessarily contain consecutive elements of the input container, but instead
  * wrap at the beginning of the $source, continuing with the last element of the latter after its first element.
  * @tparam S The kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  * @define coll reverse cyclic slice
  */
trait ReverseCyclicSliceFactory[-S[_], +C[_]] extends ReverseSizedSliceFactory[S, C] {
	/** A $coll with elements `source(first % source.size), source((first - 1) % source.size), ..., `
	  * `source(0), source(source.size - 1), ..., source((first + 1) % source.size)`.
	  * The division remainder in the listed elements is always the non-negative value, even if `first` itself is negative.
	  */
	override def from[T](source :S[T], first :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (len == 0)          make(source, 0, 0)
		else if (first >= len) make(source, first % len, len)
		else if (first < 0)    make(source, (len + first % len) % len, len)
		else                   make(source, first, len)
	}

	/** A $coll with elements of the $source in the decreasing index order, starting with `source(first % source.size)`.
	  * If `length > first + 1`, then element `source(0)` is immediately followed by `source(source.size - 1)`,
	  * `source(source.size - 2)`, and so on, until `min(length, source.size)` elements are returned.
	  * If `length` is negative, the returned $coll has no elements.
	  * If `length` is greater than the size of the input $source, then all elements of `source`
	  * are included exactly once. The division remainder in the listed elements is always the non-negative value,
	  * even if `first` itself is negative.
	  */
	override def apply[T](source :S[T], first :Int, length :Int) :C[T] = {
		val len = totalSizeOf(source)
		val from =
			if (len <= 1) 0
			else if (first >= len) first % len
			else if (first < 0) (len + first % len) % len
			else first
		make(source, from, math.min(len, math.max(length, 0)))
	}

	//An alternative approach would take size = until - from and let returning some elements more than once.
	//In that case, however, we'd need to treat both as unsigned because of a real possibility of overflow.
	/** A $coll with elements of the input $source in the reverse index order, starting with `(until - 1) % source.size`,
	  * and ending (inclusive) with `from % source.size`. If `from` is greater than `until`
	  * modulo the size of `source`, then the element at `0` is followed by elements
	  * at `source.size - 1, source.size - 2`, etc.
	  * If `from % len == until % len`, then the returned $coll will contain all elements , unless `from == until`,
	  * in which case it will be empty. The division remainder in the listed elements is always the non-negative value,
	  * even if `from` or `until` are negative.
	  */
	override def slice[T](source :S[T], from :Int, until :Int) :C[T] = {
		val len = totalSizeOf(source)
		if (len == 0) //avoid division by zero!
			make(source, 0, 0)
		else {
			val start = if (until > 0) (until - 1) % len else (len + until % len - 1) % len
			val end   = if (from > 0) (from - 1) % len else (len + from % len - 1) % len
			val size  =
				if  (start > end) start - end
				else if (start < end) len + start - end
				else if (from == until) 0
				else len
			make(source, start, size)
		}
	}
}






/** Interface of factories of collections/iterators `C[X]` over nested containers of type `S[S[X]]`.
  * @define Coll slice
  * @define coll slice
  * @define Source 2D container
  * @define source 2D container
  */
trait Slice2DFactory[-S[_], +C[_]]
	extends ExpandedSliceFactory[Pow[S]#_2 @uncheckedVariance, C]
{
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	def slice[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E]
	def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E]
}




/** The common interface of factories of collections/iterators `C[A]` flattening a slice of two-dimensional `S[S[A]]`.
  * Only the inner containers `S[A]` in the range of the created slice need to be initialized,
  * but they must all be of the same size. The exact semantics vary between implementations depending
  * on the order of including the elements. Consult the documentation of the object/value of this type for more details.
  * @note Polymorphic treatment of abstract values is inadvisable due to differing semantics.
  * @see [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.collections.CyclicMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.collections.ReverseCyclicMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.arrays.MatrixIterator$]]
  * @tparam S The linear kind of container with the elements for the slice.
  * @tparam C The kind of collections/iterators produced by this factory.
  * @define coll slice
  * @define Coll flat slice
  * @define Source 2D container
  * @define source two-dimensional container
  */
trait MatrixSliceFactory[-S[_], +C[+_]]
	extends SizedSliceFactory[Pow[S]#_2 @uncheckedVariance, C] with Slice2DFactory[S, C]
{
	/** Creates a $Coll with elements `[source(from2)(from1), source(until2)(until1))`.
	  *   - If `from2` or `until2` are greater than `source.size`, they are clipped to the actual number of elements;
	  *   - If `from2` is less than zero, then the result is the same as if both `from2` and `from1` were zero;
	  *   - If `until2` is less than zero, then an empty $coll is returned;
	  *   - If either of `from1`, `until1` are less than zero and `from2`/`until2` are positive, then the latter
	  *     is decreased by one and `from1`/`until1` becomes `source(from2).length`;
	  *   - If `from1`/`until1` is equal or greater than `source(from2).length`, then the result is the same
	  *     as if `from2`/`until2` was increased by one and `from1`/`until1` were zero.
	  * @note Some implementations return the elements of the slice in the reverse order or impose different bounds
	  *       on the indices.
	  * @param source  A two-dimensional array with iterated elements.
	  * @param from2  The outer index of the first element of `source` included in the $coll.
	  * @param from1  The inner index of the first element of `source` included in the $coll.
	  * @param until2 The outer index in `source` of the first element not included in the $coll.
	  * @param until1 The inner index in `source` of the first element not included in the $coll.
	  */
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	@throws[NullPointerException]("if source is null, or from2 < array.length and array(from2 max 0) is null.")
	override def slice[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E] = {
		//Initially I tried to implement it as @tailrec, but for some reason I could not return make(...)
		// from within it ('contains a recursive call targeting super' compile error).
		//The code is considerably more complex than the specification code because we refrain from dereferencing
		// lower dimension arrays not containing the first element of the slice (as they may be null).
		// We also avoid converting the index into a single 'flat' index as it might conceivably cause overflows.
		val length2 = outerLengthOf(source)
		var length1 = -1
		if (length2 == 0)
			return empty(source)
		var i2 = from2
		var i1 = from1
		var j2 = until2
		var j1 = until1
		while (true) {
			if (j2 < 0 | length1 == 0)
				return empty(source)
			else if (i2 < 0) {
				i2 = 0
				i1 = 0
			} else if (i1 < 0)
				if (i2 == 0)
					i1 = 0
				else {
					i2 = math.min(i2, length2) - 1
					if (length1 == -1) //We'll check for length1 == 0 first thing in the next iteration.
						length1 = outerLengthOf(get(source, i2))
					i1 = length1 - 1
				}
			else if (j1 < 0)
				if (j2 == 0)
					return empty(source)
				else {
					j2 = math.min(j2, length2) - 1
					if (length1 == -1)
						length1 = outerLengthOf(get(source, j2))
					j1 = length1 - 1
				}
			else if (i2 >= length2)
				return empty(source)
			else if (j2 >= length2) {
				j2 = length2 - 1
				if (length1 == -1)
					length1 = outerLengthOf(get(source, i2))
				j1 = length1
			} else { //Implies j2 > 0 because i2 >= 0 & i1 >= 0 & (j2 > i2 | j2 == i2 & j1 > i1).
				if (length1 == -1)
					length1 = outerLengthOf(get(source, i2))
				if (i1 >= length1) {
					i2 += 1
					i1  = 0
				} else if (j1 == 0) {
					j1 = length1
					j2 -= 1
				} else {
					if (j1 > length1)
						j1 = length1
					if (j2 < i2 | i2 == j2 & j1 <= i1)
						return empty(source)
					return make(source, i2, i1, j2, j1)
				}
			}
		}
		??!
	}

	/** A $Coll flattening a slice of an input $source.
	  * The bounds specify element indices in the linearized dimension, where the index of element `source(i)(j)`
	  * is `(i * length1 + j)`, and `length1` is the length of all the inner containers in `source` with elements
	  * in the specified range. Only the inner containers containing actual included elements need to be initialized,
	  * but they must all be of the same size (`length1`). All other inner containers
	  * are treated as if they were of size `length1` for the purpose of calculating indices.
	  * Both `from` and `until` are clipped to the valid range of `[0, source.size * length1)`.
	  * The first element in the range is `source(from / length1)(from % length1)`,
	  * and the last one is `source((until - 1) / length1)((until - 1) % length1)`.
	  * @note Some implementations return the elements of the slice in the reverse order or impose different bounds
	  *       on the indices.
	  * @param source  A $source with elements for the returned $coll.
	  * @param length1 The size of all non-null inner containers in `source.`
	  * @param from    The index of the first element in flattened `source` included in the returned $coll.
	  * @param until   The index immediately following the last element of flattened `source`
	  *                included in the returned $coll.
	  */
	@throws[NullPointerException]("if source is null or from < until and until > 0 and from < source.size * length1 " +
	                              "and array(max(from, 0) / length1) is null.")
	def slice[E](length1 :Int, source :S[S[E]] @uncheckedVariance, from :Int, until :Int) :C[E] =
		if (length1 <= 0 | from >= until | until <= 0)
			empty(source)
		else {
			val length2 = outerLengthOf(source)
			val length  = length1.toLong * length2
			val from0   = math.min(length, math.max(0, from).toLong)
			val until0  = math.min(length, math.max(from0, until))
			val from2   = from0 / length1
			val until2  = until0 / length1
			val from1   = from0 - length1 * from2
			val until1  = until0 - length1 * until2
			if (from0 == until0)
				empty(source)
			else if (until1 == 0)
				make(source, from2.toInt, from1.toInt, until2.toInt - 1, length1)
			else
				make(source, from2.toInt, from1.toInt, until2.toInt, until1.toInt)
		}

	/** A $Coll flattening a slice of an input $source.
	  * The bounds specify element indices in the linearized dimension,
	  * where the index of element `source(i)(j)` is `(i * length1 + j)`,
	  * and `length1 == source(0).size == source(from / length1).size == ... == source((until - 1) / length1).size`.
	  * Both `from` and `until` are clipped to the valid range of `[0, source.size * source(0).size]`.
	  * The first element in range is `source(from / source(0).size)(from % source(0).size)`,
	  * and the last one is `source((until - 1) / source(0).size)((until - 1) % source(0).size)`.
	  * @note Some implementations return the elements of the slice in the reverse order or impose different bounds
	  *       on the indices.
	  * @param source  A $source with elements for the returned $coll.
	  * @param from    The index of the first element in the flattened `source` included in the returned $coll.
	  * @param until   The index immediately following the last element of `source` included in the returned $coll.
	  */
	@throws[NullPointerException]("if from < until and either source is null or source(0) is null.")
	override def slice[E](source :S[S[E]] @uncheckedVariance, from :Int, until :Int) :C[E] =
		if (from >= until | until < 0 | outerLengthOf(source) == 0)
			empty(source)
		else
			slice(outerLengthOf(get(source, 0)), source, from, until)


	/** A $Coll containing `size` elements of `source` starting with `source(from2)(from1)`.
	  *   - If `size < 0` or `source.size == 0` then an empty $coll is returned immediately.
	  *   - If `from2 < 0` then both `from2` and `from1` are set to zero.
	  *   - If `from2 > source.size`, it is set to `source.size`.
	  *   - If `from1 < 0`, then `from2` is decreased by one and `from1` becomes `source(from2).size`
	  *     (negative `from2` handled as before).
	  *   - If `from2 == source.size` an empty $coll is returned.
	  *   - If `from1 >= source(from2).length`, then `from2` is increased by one (overflow treated as before)
	  *     and `from1` is set to zero.
	  *   - If `size > (source.size - from2) * source(from2).size - from1`,
	  *     it is clipped to the number if available elements.
	  *
	  * Only the inner containers with actually included elements need to be initialized,
	  * but they must all be of the same size.
	  * @note Some implementations include elements preceding `source(from2)(from1)` rather than following it,
	  *       or wrap at the beginning/end of the outer container.
	  * @param source A $source with elements included in the returned $coll.
	  * @param from2 The index in `source` of the inner container with the first included element.
	  * @param from1 The index in `source(from2)` of the first included element.
	  * @param size  The maximum number of elements in the returned $coll.
	  */
	@throws[NullPointerException]("if source is null or from2 < source.length and source(from2) is null.")
	override def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E] = {
		//Initially I tried to implement it as @tailrec, but for some reason I could not return make(...)
		// from within it ('contains a recursive call targeting super' compile error).
		//The code is considerably more complex than the specification code because we refrain from dereferencing
		// lower dimension arrays not containing the first element of the slice (as they may be null).
		// We also avoid converting the index into a single 'flat' index as it might conceivably cause overflows.
		var idx2 = from2
		var idx1 = from1
		val length2 = outerLengthOf(source)
		if (size <= 0 | length2 == 0)
			return empty(source)
		var length1 = -1
		while (true) { //Clip idx2 and idx1 to range.
			if (length1 == 0)
				return empty(source)
			else if (idx2 < 0) {
				idx2 = 0
				idx1 = 0
			} else if (idx1 < 0)
				if (idx2 == 0)
					idx1 = 0
				else {
					idx2 = math.min(length2, idx2) - 1
					if (length1 == -1)
						length1 = outerLengthOf(get(source, idx2))
					idx1 = length1 - 1
				}
			else if (idx2 >= length2) //Covers also length2 == 0 because idx2 >= 0
				return empty(source)
			else {
				if (length1 == -1)
					length1 = outerLengthOf(get(source, idx2))
				if (idx1 >= length1)  //Covers also length1 == 0 because idx1 >= 0.
					if (idx2 == length2 - 1)
						return empty(source)
					else {
						idx2 += 1
						idx1 = 0
					}
				else {
					val size0 = math.min(size, (length2 - idx2).toLong * length1 - idx1).toInt
					return make(source, idx2, idx1, size0)
				}
			}
		}
		??!
	}

	/** A $Coll containing `size` elements of `source` starting with `source(from / length1)(from % length1)`.
	  * All inner containers with included elements must be of size `length1`. All other inner containers
	  * are treated as if they were of size `length1` for the purpose of calculating indices.
	  * If `source.size == 0` or `length1 <= 0`, an empty $coll is returned immediately.
	  * If `from` is less than zero or greater than `source.flatten.size` it is clipped to range `[0..source.size`
	  * @param length1 The length of the inner containers in `source`.
	  * @param source  A $source with the elements for the returned $coll.
	  * @param from    An index of the first element in flattened `source` included in the returned $coll.
	  * @param size    The maximum number of included elements.
	  */
	@throws[NullPointerException]("if source is null or from2 < source.length and source(from2) is null.")
	def apply[E](length1 :Int, source :S[S[E]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (length1 <= 0 | size <= 0)
			empty(source)
		else if (from < 0)
			apply(source, -1, 0, size)
		else {
			val from2 = from / length1
			val from1 = from - from2 * length1
			apply(source, from2, from1, size)
		}

	/** A $Coll iterating over at most `size` elements of `array`,
	  * starting with `array(from / array(0).length)(from % array(0).length)`.
	  * @param source   A two-dimensional array with the iterated elements.
	  * @param from    A valid index of the first element of the iterator
	  *                in a continuous range `[0, array.length*length1]`.
	  * @param size    The maximum number of iterated elements.
	  */
	@throws[NullPointerException]("if array is null or array(0) is null.")
	@throws[IndexOutOfBoundsException]("if from < 0 or from > array.length * length1.")
	override def apply[E](source :S[S[E]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (size <= 0 | outerLengthOf(source) == 0)
			empty(source)
		else
			apply(outerLengthOf(get(source, 0)), source, from, size)

	override def apply[E](source :S[S[E]] @uncheckedVariance) :C[E] = {
		val len2 = outerLengthOf(source)
		if (len2 == 0)
			Empty
		else {
			val len1 = outerLengthOf(get(source, 0))
			if (len1 == 0)
				empty(source)
			else
				make(source, 0, 0, len2 * len1)
		}
	}


	protected def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E] =
		make(source, from2, from1, (until2 - from2) * outerLengthOf(get(source, from2)) + until1 - from1)

	protected def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E]

	protected override def make[T](source :S[S[T]] @uncheckedVariance, first :Int, size :Int) :C[T] = {
		val length2 = outerLengthOf(source)
		if (length2 == 0 | size <= 0)
			empty(source)
		else {
			val length1 = outerLengthOf(get(source, 0))
			if (length1 == 0)
				empty(source)
			else {
				val from2 = first / length1
				val from1 = first - from2 * length1
				make(source, from2, from1, size)
			}
		}
	}


	protected def empty[E](array :S[S[E]] @uncheckedVariance) :C[E] = Empty
	protected def empty[E] :C[E] = Empty

	protected val Empty :C[Nothing]

	protected final override def totalSizeOf[E](source :S[S[E]] @uncheckedVariance) :Int = {
		val len2 = outerLengthOf(source)
		if (len2 == 0) 0
		else outerLengthOf(get(source, 0)) * len2
	}
	//Consider: replacing these with LikeIndexedSeq.Generic[S]
	protected def outerLengthOf[E](source :S[E]) :Int
	protected def get[E](source :S[E], index :Int) :E
}




private[sugar] trait ReverseMatrixSliceFactory[-S[_], +C[+_]]
	extends MatrixSliceFactory[S, C]
	   with ReverseSizedSliceFactory[Pow[S]#_2 @uncheckedVariance, C]
{
	protected override def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
		:C[E] =
	{
		val innerLength = outerLengthOf(get(source, from2))
		make(source, until2, until1 - 1, (until2 - from2) * innerLength - from1 + until1)
	}

	override def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E] = {
		val length2 = outerLengthOf(source)
		if (size <= 0 | length2 == 0)
			return empty(source)
		var length1 = -1
		var i2 = from2
		var i1 = from1
		while (true) {
			if (i2 < 0 | length1 == 0)
				return empty(source)
			if (i1 < 0)
				if (i2 == 0)
					return empty(source)
				else {
					i2 = math.min(i2, length2) - 1
					if (length1 == -1)
						length1 = outerLengthOf(get(source, i2))
					i1 = length1 - 1
				}
			else if (i2 >= length2) {
				i2 = length2 - 1
				if (length1 == -1)
					length1 = outerLengthOf(get(source, i2))
				i1 = length1 - 1
			} else {
				if (length1 == -1)
					length1 = outerLengthOf(get(source, i2))
				if (i1 >= length1) {
					i2 += 1
					i1 = 0
				} else {
					val size0 = math.min(size, i2.toLong * length1 + i1 + 1).toInt
					return make(source, i2, i1, size0)
				}
			}
		}
		??!
	}
}




private[sugar] trait AbstractCyclicMatrixSliceFactory[-S[_], +C[+_]]
	extends MatrixSliceFactory[S, C]
{
	/** A $coll over a slice of `array`, starting at indices `from2` and `from1` in the outer and inner array,
	  * respectively, and ending immediately before indices `(until2, until1)`. Indices in the outer array
	  * - `from2` and `until2` - are always treated modulo the length of the array. Indices in the inner arrays
	  * - `from1` and `until1` - are instead clipped to range `[0, length]`, where `length` is the length
	  * of all element arrays in `array`. If `from1` or `until1` equals `length` (after clipping), it is set to zero,
	  * an `from2`/`until2` is increased by one modulo `array.length`. If `until2 * length + until1` is less than
	  * `from2 * length + from1` after all these operations, the iteration will wrap at the end of the array
	  * and continue from the beginning (or end, in case of `ReverseCyclicMatrixIterator`). If `from2 == until2`,
	  * and `from1` equals `until1` after clipping to range `[0, length]`, the iterator is empty. Otherwise,
	  * if `from2` equals `until2` modulo `array.length`, the iterator will return all elements in the array.
	  */
	override def slice[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E] = {
		val length2 = outerLengthOf(source)
		if (length2 == 0)
			empty(source)
		else {
			//We need to treat the negative case separately because the expression would overflow for positive values.
			val length2L         = length2.toLong
			val length2Multiple  = length2L << 32 //A multiple greater than Int.MaxValue * 2 to assure positive signs.
			var clippedFrom2     = (length2Multiple + from2) % length2L
			var clippedUntil2    = (length2Multiple + until2) % length2L
			var clippedFrom1     = math.max(from1, 0)
			var clippedUntil1    = math.max(until1, 0)
			val length1          = outerLengthOf(get(source, clippedFrom2.toInt))
			var from2Adjustment  = 0L
			if (from1 >= length1) {
				clippedFrom2 = (clippedFrom2 + 1) % length2L
				clippedFrom1 = 0
				from2Adjustment = 1L
			}
			var until2Adjustment = 0L
			if (until1 >= length1) {
				clippedUntil2 = (clippedUntil2 + 1) % length2L
				clippedUntil1 = 0
				until2Adjustment = 1L
			}
			if (length1 == 0 || from2 + from2Adjustment == until2 + until2Adjustment && clippedFrom1 == clippedUntil1)
				empty(source)
			else
				make(source, clippedFrom2.toInt, clippedFrom1, clippedUntil2.toInt, clippedUntil1)
		}
	}
	override def slice[E](length1 :Int, source :S[S[E]] @uncheckedVariance, from :Int, until :Int) :C[E] =
		if (length1 == 0 || from == until)
			empty(source)
		else {
			val length2 = outerLengthOf(source)
			val length  = length2 * length1
			var from0   = from % length
			if (from0 < 0)
				from0 = length + from0
			var until0 = until % length
			if (until0 < 0)
				until0 = length + until0
			val from2  = from0 / length1
			val from1  = from0 - from2 * length1
			var until2 = (until0 - 1) / length1
			val until1 = until0 - until2 * length1
			if (until2 < 0)
				until2 += length2
			make(source, from2, from1, until2, until1)
		}

	override def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E] = {
		val outerLength = outerLengthOf(source)
		if (outerLength == 0 || size <= 0)
			empty(source)
		else {
			val clippedFrom2 =
				if (from2 >= 0) from2 % outerLength
				else (outerLength + from2 % outerLength) % outerLength
			val innerLength  = outerLengthOf(get(source, clippedFrom2))
			if (innerLength == 0)
				empty(source)
			else if (from1 >= innerLength)
				apply(source, clippedFrom2 + 1, 0, size)
			else {
				val totalLength  = outerLength * innerLength
				val clippedSize  = math.max(0, math.min(size, totalLength))
				make(source, clippedFrom2, math.max(0, from1), clippedSize)
			}
		}
	}
	override def apply[E](length1 :Int, source :S[S[E]] @uncheckedVariance, from :Int, size :Int) :C[E] = {
		val length2 = outerLengthOf(source)
		if (length2 == 0 | length1 <= 0 | size <= 0)
			empty(source)
		else {
			val totalLength = length2.toLong * length1
			val from0 =
				if (from < 0) totalLength + from % totalLength
				else from % totalLength
			val from2 = from0 / length1
			val from1 = from0 - from2 * length1
			apply(source, from2.toInt, from1.toInt, size)
		}
	}
}


private[sugar] trait CyclicMatrixSliceFactory[-S[_], +C[+_]]
	extends AbstractCyclicMatrixSliceFactory[S, C]
	   with CyclicSliceFactory[Pow[S]#_2 @uncheckedVariance, C]
{
	protected override def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:C[E] =
	{
		val outerLength = outerLengthOf(source).toLong
		val innerLength = outerLengthOf(get(source, from2)).toLong
		val from = from2 * innerLength + from1
		val until = until2 * innerLength + until1
		if (from < until)
			make(source, from2, from1, (until - from).toInt)
		else
			make(source, from2, from1, (outerLength * innerLength + until - from).toInt)
	}

}


private[sugar] trait ReverseCyclicMatrixSliceFactory[-S[_], +C[+_]]
	extends AbstractCyclicMatrixSliceFactory[S, C]
	   with ReverseCyclicSliceFactory[Pow[S]#_2 @uncheckedVariance, C]
{
	protected override def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
			:C[E] =
	{
		val outerLength = outerLengthOf(source)
		val innerLength = outerLengthOf(get(source, from2)).toLong
		val from = from2 * innerLength + from1
		val until = until2 * innerLength + until1
		val size =
			if (from < until) (until - from).toInt
			else (outerLength * innerLength + until - from).toInt
		if (until1 > 0)
			make(source, until2, until1 - 1, size)
		else if (until2 == 0)
			make(source, outerLength - 1, (innerLength - 1).toInt, size)
		else
			make(source, until2 - 1, (innerLength - 1).toInt, size)
	}
}




/** Interface of factories of collections/iterators `C[X]` over nested containers of type `S[S[S[X]]]`.
  * @define coll 3d slice
  * @define source 3D container
  * @define source 3D container
  */
trait Slice3DFactory[-S[_], +C[_]]
	extends ExpandedSliceFactory[Pow[S]#_3 @uncheckedVariance, C]
{
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	def slice[E](array :S[S[S[E]]] @uncheckedVariance,
	             from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E]

	def apply[E](array :S[S[S[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E]
}
