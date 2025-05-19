package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.StrictOptimizedSeqFactory

import net.noresttherein.sugar.exceptions.??!
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
  * @define polymorphismNote Due to varying semantics, polymorphic use of abstract values is inadvisable.
  * @define linearIndexingNote Factory methods inherited from
  *                            [[net.noresttherein.sugar.collections.ExpandedSliceFactory ExpandedSliceFactory]]
  *                            (using single-dimensional indexing in the flattened $source) typically must access
  *                            the first element of collections of dimensions two and three in order to determine
  *                            their lengths and the indices for each dimension. For this reason they are unsuitable
  *                            for sparse collections, where some members may be not initialized.
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
  * @note $polymorphismNote
  * @note $linearIndexingNote
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
  * @note $polymorphismNote
  * @note $linearIndexingNote
  * @see [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.collections.CyclicMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.collections.ReverseCyclicMatrixSliceFactory]]
  * @see [[net.noresttherein.sugar.arrays.MatrixIterator$]]
  * @tparam S The kind of container with the elements for the slice.
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
	  * If the outer or the inner container is empty, an empty $coll is returned.
	  * Index pairs `(from2, from1)` and `(until2, until1)` are adjusted to range in the following manner:
	  *   - If any of `from2` or `until2` are greater than the (outer) size of `source`, they are reduced to its size.
	  *   - If either `from1` or `until1` are negative, then they are set to the size of the inner container minus one,
	  *     and the corresponding index of the outer dimension is decreased.
	  *   - If either `from1` or `until1` are greater than the size of inner containers, they are set to zero,
	  *     and the corresponding index of the outer index is increased.
	  *
	  * The returned $coll contains all individual elements of $source whose index pairs are lexicographically
	  * not lesser than modified `(from2, from1)` and lesser than `(until2, until1)`.
	  * @note If the resulting $coll is empty, but `(from2, from1)` is lexicographically greater than `(until2, until1)`
	  *       (before modifications), the method will access the inner container corresponding to the element
	  *       immediately preceding the upper bound index in order to check its size. This will result in an error
	  *       (such as a `NullPointerException`) being raised if the inner container is not initialized.
	  * @note [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory Reverse]] slices contain the same elements,
	  *       but in the reverse order.
	  * @note [[net.noresttherein.sugar.collections.AbstractCyclicMatrixSliceFactory Cyclic]] slices instead treat
	  *       out of range indices of the second dimension as modulo the size of `source`.
	  * @param source A two-dimensional array with iterated elements.
	  *               All inner containers with elements in the specified range must have the same size.
	  * @param from2  The outer index of the first element of `source` included in the $coll.
	  * @param from1  The inner index of the first element of `source` included in the $coll.
	  * @param until2 The outer index in `source` of the first element not included in the $coll.
	  * @param until1 The inner index in `source` of the first element not included in the $coll.
	  */
	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.
	@throws[NullPointerException]("if source is null, or from2 < array.length and array(from2 max 0) is null.")
	override def slice[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E] = {
		//todo: rewrite it in the manner after CuboidSliceFactory
		//Initially I tried to implement it as @tailrec, but for some reason I could not return make(...)
		// from within it ('contains a recursive call targeting super' compile error).
		//The code is considerably more complex than the specification code because we refrain from dereferencing
		// lower dimension arrays not containing the first element of the slice (as they may be null).
		// We also avoid converting the index into a single 'flat' index as it might conceivably cause overflows.
		val length2 = lengthOf(source)
		var length1 = -1
		//This is far from an exhausting condition for an empty slice, but we need to provide at least some way
		// for the caller to create a valid iterator without an error even if all inner arrays are null.
		if (length2 == 0 | from2 == until2 & from1 >= until1)
			return empty(source)
		var i2 = from2
		var i1 = from1
		var j2 = until2
		var j1 = until1
		while (true) {
			if (j2 < 0 | length1 == 0)
				return empty(source)
			if (i2 < 0) {
				i2 = 0
				i1 = 0
			} else if (i1 < 0)
				if (i2 == 0)
					i1 = 0
				else {
					i2 = math.min(i2, length2) - 1
					if (length1 == -1) //We'll check for length1 == 0 the first thing in the next iteration.
						length1 = lengthOf(get(source, i2))
					i1 = length1 - 1
				}
			else if (j1 < 0)
				if (j2 == 0)
					return empty(source)
				else {
					j2 = math.min(j2, length2) - 1
					if (length1 == -1)
						length1 = lengthOf(get(source, j2))
					j1 = length1 - 1
				}
			else if (i2 >= length2)
				return empty(source)
			else if (j2 >= length2) {
				j2 = length2 - 1
				if (length1 == -1)
					length1 = lengthOf(get(source, i2))
				j1 = length1
			} else {
				if (length1 == -1)
					length1 = lengthOf(get(source, i2))
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
					//consider: adding length1 as a parameter
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
	def slice[E](length1 :Int, source :S[S[E]] @uncheckedVariance, from :Int, until :Int) :C[E] = {
		if (length1 <= 0 | from >= until | until <= 0)
			return empty(source)
		val length2 = lengthOf(source)
		val length  = length1.toLong * length2
		val from0   = math.min(length, math.max(0, from).toLong)
		val until0  = math.min(length, math.max(from0, until))
		if (from0 >= until0)
			return empty(source)
		val from2   = from0 / length1
		val until2  = until0 / length1
		val from1   = from0 - length1 * from2
		val until1  = until0 - length1 * until2
		if (from0 >= until0)
			empty(source)
		else if (until1 == 0) //todo: add length1 parameter
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
		if (from >= until | until < 0 | lengthOf(source) == 0)
			empty(source)
		else
			slice(lengthOf(get(source, 0)), source, from, until)


	/** A $Coll containing `size` elements of `source` starting with `source(from2)(from1)`.
	  *   If `size <= 0`, or the container of any dimension is empty, then an empty $coll is returned.
	  *   Otherwise, the individual index values are adjusted to range according to the following rules:
	  *   - Negative `from2` results in the $coll containing the first `size` elements of the whole $source,
	  *     or being empty for [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory reverse]] slices.
	  *   - An index greater or equal than the size of the corresponding container is clipped to its size.
	  *   - Negative `from1` specifies the last individual element `E` in the preceding container
	  *     (or before the whole $source is `from2` is zero, as per the previous rule).
	  *     The returned $coll will contain the first `size` elements of the whole $source
	  *     (when treated as a full matrix). [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory Reverse]]
	  *     slices are an exception, and en empty $coll is returned instead.
	  *   - If the original `from1` is equal or greater than the size of its container, then it is set to zero
	  *     and `from2` is increased by one.
	  *   - If `from2` after modification is equal or greater than the size of the outer container,
	  *     then an empty $coll is returned. [[net.noresttherein.sugar.collections.ReverseMatrixSliceFactory Reverse]]
	  *     slices are an exception, and contain the last `size` elements of the whole $source in that case.
	  *
	  * Only the inner containers with actually included elements need to be initialized,
	  * but they must all be of the same size.
	  * @note [[net.noresttherein.sugar.collections.AbstractCyclicMatrixSliceFactory Cyclic]] slices apply
	  *       [[[net.noresttherein.sugar.collections.AbstractCyclicMatrixSliceFactory Cyclic.apply[E](source:S[S[E]],from2* different]]]
	  *       rules for indices out of range, and wrap at the end/beginning of the whole $source.
	  * @param source A $source with elements included in the returned $coll.
	  * @param from2 The index in `source` of the inner container with the first included element.
	  * @param from1 The index in `source(from2)` of the first included element.
	  * @param size  The maximum number of elements in the returned $coll.
	  */
	@throws[NullPointerException]("if source is null or from2 < source.length and source(from2) is null.")
	override def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E] = {
		//todo: rewrite it in the manner after CuboidSliceFactory
		//Initially I tried to implement it as @tailrec, but for some reason I could not return make(...)
		// from within it ('contains a recursive call targeting super' compile error).
		//The code is considerably more complex than the specification code because we refrain from dereferencing
		// lower dimension arrays not containing the first element of the slice (as they may be null).
		// We also avoid converting the index into a single 'flat' index as it might conceivably cause overflows.
		var idx2 = from2
		var idx1 = from1
		val length2 = lengthOf(source)
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
						length1 = lengthOf(get(source, idx2))
					idx1 = length1 - 1
				}
			else if (idx2 >= length2)
				return empty(source)
			else {
				if (length1 == -1)
					length1 = lengthOf(get(source, idx2))
				if (idx1 >= length1)  //Covers also length1 == 0 because idx1 >= 0.
					if (idx2 == length2 - 1)
						return empty(source)
					else {
						//This case is problematic because we have accessed source(idx2)
						// despite it not being actually used by the iterator, and thus conceivably null.
						// The only way to avoid is to not increase idx2, but instead clip idx1 to length1 - 1,
						// but it would be unintuitive and quite inconsistent with slice.
						// At the minimum, we should document this issue.
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
	@throws[NullPointerException]("if source is null or source(0) is null.")
	override def apply[E](source :S[S[E]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (size <= 0 | lengthOf(source) == 0)
			empty(source)
		else
			apply(lengthOf(get(source, 0)), source, from, size)

	override def apply[E](source :S[S[E]] @uncheckedVariance) :C[E] = {
		val len2 = lengthOf(source)
		if (len2 == 0)
			Empty
		else {
			val len1 = lengthOf(get(source, 0))
			if (len1 == 0)
				empty(source)
			else
				make(source, 0, 0, len2 * len1)
		}
	}


	protected def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int) :C[E] =
		make(source, from2, from1, (until2 - from2) * lengthOf(get(source, from2)) + until1 - from1)

	protected def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E]

	protected override def make[T](source :S[S[T]] @uncheckedVariance, first :Int, size :Int) :C[T] = {
		val length2 = lengthOf(source)
		if (length2 == 0 | size <= 0)
			empty(source)
		else {
			val length1 = lengthOf(get(source, 0))
			if (length1 == 0)
				empty(source)
			else {
				val from2 = first / length1
				val from1 = first - from2 * length1
				make(source, from2, from1, size)
			}
		}
	}


	protected def empty[E](source :S[S[E]] @uncheckedVariance) :C[E] = Empty
	protected def empty[E] :C[E] = Empty

	protected val Empty :C[Nothing]

	protected final override def totalSizeOf[E](source :S[S[E]] @uncheckedVariance) :Int = {
		val len2 = lengthOf(source)
		if (len2 == 0) 0
		else lengthOf(get(source, 0)) * len2
	}
	//Consider: replacing these with LikeIndexedSeq.Generic[S]
	protected def lengthOf[E](source :S[E]) :Int
	protected def get[E](source :S[E], index :Int) :E
}




private[sugar] trait ReverseMatrixSliceFactory[-S[_], +C[+_]]
	extends MatrixSliceFactory[S, C]
	   with ReverseSizedSliceFactory[Pow[S]#_2 @uncheckedVariance, C]
{
	protected override def make[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, until2 :Int, until1 :Int)
		:C[E] =
	{
		val innerLength = lengthOf(get(source, from2))
		make(source, until2, until1 - 1, (until2 - from2) * innerLength - from1 + until1)
	}

	/** A $Coll containing `size` elements of `source` preceding and including `source(from2)(from1)`, in reverse order.
	  *   - If `size < 0` or `source.size == 0` then an empty $coll is returned immediately.
	  *   - If `from2 < 0` or `from2 == 0` and `from1 < 0` then an empty $coll is returned.
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
	override def apply[E](source :S[S[E]] @uncheckedVariance, from2 :Int, from1 :Int, size :Int) :C[E] = {
		val length2 = lengthOf(source)
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
						length1 = lengthOf(get(source, i2))
					i1 = length1 - 1
				}
			else if (i2 >= length2) {
				i2 = length2 - 1
				if (length1 == -1)
					length1 = lengthOf(get(source, i2))
				i1 = length1 - 1
			} else {
				if (length1 == -1)
					length1 = lengthOf(get(source, i2))
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
		val length2 = lengthOf(source)
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
			val length1          = lengthOf(get(source, clippedFrom2.toInt))
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
			val length2 = lengthOf(source)
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
		val outerLength = lengthOf(source)
		if (outerLength == 0 || size <= 0)
			empty(source)
		else {
			val clippedFrom2 =
				if (from2 >= 0) from2 % outerLength
				else (outerLength + from2 % outerLength) % outerLength
			val innerLength  = lengthOf(get(source, clippedFrom2))
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
		val length2 = lengthOf(source)
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
		val outerLength = lengthOf(source).toLong
		val innerLength = lengthOf(get(source, from2)).toLong
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
		val outerLength = lengthOf(source)
		val innerLength = lengthOf(get(source, from2)).toLong
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
  * @note $polymorphismNote
  * @note $linearIndexingNote
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




/** The common interface of factories of collections/iterators over three-dimensional containers.
  * Only the inner arrays which contain included elements must be initialized,
  * but all non-null arrays on the second and third level must all be of the same length.
  * The exact semantics vary between implementations depending on the order of visiting the elements.
  * Consult the documentation of the object/value of this type for more details.
  * @note $polymorphismNote
  * @note $linearIndexingNote
  * @see [[net.noresttherein.sugar.collections.ReverseCuboidSliceFactory]]
  * @see [[net.noresttherein.sugar.arrays.CyclicCuboidSliceFactoru]]
  * @see [[net.noresttherein.sugar.arrays.ReverseCyclicCuboidSliceFactory]]
  * @see [[net.noresttherein.sugar.arrays.CuboidIterator$]]
  * @tparam S the container kind with elements for the collection.
  * @tparam C the kind of the crated collection.
  * @define coll slice
  * @define Coll flat slice
  * @define Source 3D container
  * @define source three-dimensional container
  */
trait CuboidSliceFactory[-S[_], +C[+_]]
	extends SizedSliceFactory[Pow[S]#_3 @uncheckedVariance, C] with Slice3DFactory[S, C]
{
	//The rule we have to adhere to in semantics is that the length of a lower dimension array is not necessary
	// to unequivocally determine the index of a higher dimension except of the 'until' element
	// if it is the first element of the succeeding array (i.e., until3, length2 is fine despite technically
	// pointing to (until3 + 1, 0), because the element is never accessed.

	@throws[NullPointerException]("if source or any of lower dimension containers to which 'until' indices point is null.")
	override def slice[E](source :S[S[S[E]]] @uncheckedVariance,
	                      from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val length3 = lengthOf(source)
		//A way for callers to successfully create an iterator even if all 2d arrays are null.
		if (length3 == 0 | until3 <= from3 & until2 <= from2 & until1 <= from1)
			return empty(source)
		var i3 = from3
		var i2 = from2
		var i1 = from1
		var j3 = until3
		var j2 = until2
		var j1 = until1
		/* We must treat (j3, -1, -1) the same as (j3, -1, x) because if length2 == 1 then double decrement
		 * would bring us below zero once j3 is already picked and source(j3) accessed.
		 * This could only be saved if i3 < j3, so the array would be in range, but we can't guarantee it.
		 * The only question is if it's more intuitive to always set j1 = length1 - 1,
		 * or only clip it to [0, length1 - 1].
		 * An even bigger issue is (j3, length2, length1), because we have no way of knowing if j2 and j1
		 * are greater than respective array lengths before accessing source(j3).
		 * That's why we use until rather than from: because the upper bound is exclusive,
		 * source(j3++) is out of range only for empty iterators. That's also why (j3, length2, length1)
		 * should best be equivalent to (j3, length2, 0), as it reduces the chances of i3 == j3 & i2 == length2.
		 */
		//The core principle is that if an index of a lower dimension N should be set to lengthN - 1
		// (before we know lengthN), we set it to -1 for the time being but adjust indices of higher dimensions ASAP.
		// An indexN lesser than zero also automatically sets all lower indices to -1 with the above in mind.
		// An indexN greater than lengthN results in setting it, and all lower indices to zero.

		//Pin down i3 and j3.
		if (i2 < 0 | i2 == 0 & i1 < 0)
			i3 = math.min(length3, math.max(0, i3)) - 1
		else if (i3 >= length3) {
			i3 = length3 - 1
			i2 = Int.MaxValue
		}
		if (j2 < 0 | j2 == 0 & j1 <= 0)
			j3 = math.min(length3, math.max(0, j3)) - 1 //+ j2
		else if (j3 >= length3) {
			j3 = length3 - 1
			j2 = Int.MaxValue
		}
		if (j3 < 0)
			return empty(source)
		if (i3 < 0) {
			i3 = 0
			i2 = 0
			i1 = 0
		}
		var a2 = get(source, j3)
		if (a2 == null && j3 < length3 - 1)
			a2 = get(source, j3 + 1)
		val length2 = lengthOf(a2)
		if (length2 == 0 | j3 < i3) //i3 may yet increase, but not decrease. j3 is final.
			return empty(source)

		//Pin down i2 and j2.
		if (i1 < 0)
			if (i2 <= 0)
				i2 = length2 - 1 //We have already decreased i3
			else
				i2 = math.min(length2, i2) - 1
		else if (i2 < 0) {
			i2 = length2 - 1
			i1 = -1             //Mark for setting it to length1 - 1 later.
		} else if (i2 >= length2) {
			i2 = length2 - 1
			i1 = Int.MaxValue
		}
		if (j2 < 0) {
			j2 = length2 - 1
			j1 = -1
		} else if (j1 <= 0) {
			if (j1 == 0)        //Make it easy for ReverseCuboidSliceFactory.make to obtain indices of the first element.
				j1 = Int.MaxValue
			if (j2 == 0)
				j2 = length2 - 1
			else
				j2 = math.min(length2, j2) - 1
		} else if (j2 >= length2) {
			j2 = length2 - 1
			j1 = Int.MaxValue
		}
		var a1 = get(a2, j2)
		if (a1 == null && j2 < length2 - 1)
			a1 = get(a2, j2 + 1)
		val length1 = lengthOf(a1)
		if (length1 == 0 | i3 == j3 & j2 < i2) //i2 may yet increase, but not decrease. j2 will not change.
			return empty(source)

		//Pin down i1 and j1.
		if (i1 < 0)
			i1 = length1 - 1
		j1 = if (j1 < 0) length1 - 1 else math.min(length1, j1)
		if (i3 == j3 & i2 == j2 & j1 <= i1)
			return empty(source)

		if (i1 >= length1) { //Fine for the upper bound, not the lower bound. Adjust to a valid index.
			i1 = 0
			i2 += 1
			if (i2 == length2) {
				i2 = 0
				i3 += 1
				if (i3 == length3)
					return empty(source)
			}
		}
		if (j1 == 0) {
			j1 = length1
			j2 -= 1
			if (j2 < 0) {
				j2 = length2 - 1
				j3 -= 1
			}
		}
		make(length2, length1, source, i3, i2, i1, j3, j2, j1)
	}

	@throws[NullPointerException]("if until > from, length2, length1 > 0 and either source " +
	                              "or any of the lower dimension containers corresponding to 'from' is null.")
	def slice[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance, from :Int, until :Int) :C[E] = {
		if (length2 <= 0 | length1 <= 0 | from > until | until <= 0)
			return empty(source)
		val length3  = lengthOf(source)
		val length12 = length1.toLong * length2
		val length   = length3 * length12
		val from0    = math.min(length, math.max(0, from).toLong)
		val until0   = math.min(length, math.max(from0, until))
		if (from0 >= until0)
			return empty(source)
		val from3    = (from0 / length12).toInt
		val from21   = from0 - from3 * length12
		val from2    = (from21 / length1).toInt
		val from1    = (from21 - from2 * length1).toInt
		val until3   = (until0 / length12).toInt
		val until21  = until0 - until3 * length12
		val until2   = (until21 / length1).toInt
		val until1   = (until21 - length1 * until2).toInt
		if (until1 == 0)
			if (until2 == 0)
				make(length2, length1, source, from3, from2, from1, until3 - 1, length2 - 1, length1)
			else
				make(length2, length1, source, from3, from2, from1, until3, until2 - 1, length1)
		else
			make(length2, length1, source, from3, from2, from1, until3, until2, until1)
	}

	@throws[NullPointerException]("if from < until and one of source, source.head, source.head.head is null.")
	override def slice[E](source :S[S[S[E]]] @uncheckedVariance, from :Int, until :Int) :C[E] =
		if (from >= until | until < 0 | lengthOf(source) == 0)
			empty(source)
		else {
			val a2   = get(source, 0)
			val len2 = lengthOf(a2)
			if (len2 == 0)
				empty(source)
			else
				slice(len2, lengthOf(get(a2, 0)), source, from, until)
		}


	@throws[NullPointerException]("if one of source, source(from3), source(from3)(from2) is null.")
	override def apply[E](source :S[S[S[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] =
		lengthOf(source) match {
			case       0 => empty(source)
			case length3 =>
				if (size <= 0)
					return empty(source)
				var i3 = from3
				var i2 = from2
				var i1 = from1
				if (i2 < 0 | i2 == 0 & i1 < 0)
					i3 = math.min(length3, math.max(0, i3)) - 1
				else if (i3 >= length3) {
					i3 = length3 - 1
					i2 = Int.MaxValue
				}
				if (i3 < 0) {
					i3 = 0
					i2 = 0
					i1 = 0
				}
				var a2 = get(source, i3)
				//Try to save the situation in case i2 >= length2
				if (a2 == null && i3 < length3 - 1)
					a2 = get(source, i3 + 1)
				lengthOf(a2) match {
					case 0       => empty(source)
					case length2 =>
						if (i1 < 0)
							if (i2 <= 0)
								i2 = length2 - 1 //We have already decreased i3
							else
								i2 = math.min(length2, i2) - 1
						else if (i2 < 0) {
							i2 = length2 - 1
							i1 = -1             //Mark for setting it to length1 - 1 later.
						} else if (i2 >= length2) {
							i2 = length2 - 1
							i1 = Int.MaxValue
						}
						var a1 = get(a2, i2)
						if (a1 == null && i2 < length2 - 1)
							a1 = get(a2, i2 + 1)
						lengthOf(a1) match {
							case       0 => empty(source)
							case length1 =>
								if (i1 < 0)
									i1 = length1 - 1
								else if (i1 >= length1) {
									i1 = 0
									i2 += 1
									if (i2 == length2) {
										i2 = 0
										i3 += 1
									}
								}
								apply(length2, length1, source, i3, i2, i1, size)
						}
				}
		}

	@throws[NullPointerException]("if source is null or its element containing the from-th element is null.")
	def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (length2 <= 0 | length1 <= 0 | size <= 0)
			empty(source)
		else if (from < 0)
			apply(length2, length1, source, 0, 0, 0, size) //This works for both backwards and regular slices, but it's a shame to discard lengths.
		else {
			val length3  = lengthOf(source)
			val length21 = length2.toLong * length1
			val from3    = (from / length21).toInt
			if (from3 >= length3)
				return apply(length2, length1, source, length3, 0, 0, size)
			val rem2  = from - from3 * length21
			val from2 = (rem2 / length1).toInt
			val from1 = (rem2 - from2 * length1).toInt
			apply(length2, length1, source, from3, from2, from1, size)
		}

	protected def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                       from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] =
	{
		val size3 = lengthOf(source) - from3 - 1
		val size2 = length2 - from2 - 1
		val size1 = length1 - from1
		val size0 = math.min(size, (size3 * length2.toLong + size2) * length1 + size1).toInt
		size0 match {
			case 0 => empty(source)
			case _ => make(source, from3, from2, from1, size0)
		}
	}

	@throws[NullPointerException]("if array is null or array(0) is null.")
	override def apply[E](source :S[S[S[E]]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (size <= 0 | lengthOf(source) == 0)
			empty(source)
		else {
			val a2   = get(source, 0)
			val len2 = lengthOf(a2)
			if (len2 == 0)
				empty(source)
			else
				apply(len2, lengthOf(get(a2, 0)), source, from, size)
		}

	override def apply[E](source :S[S[S[E]]] @uncheckedVariance) :C[E] =
		totalSizeOf(source) match {
			case 0 => empty(source)
			case n => make(source, 0, 0, 0, n)
		}


	//Use of @uncheckedVariance is sound because the only non-covariant ArrayLike subtypes
	// are MutableArray (we won't create an instance for that type) and `Array`.

	protected def make[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                      from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val size3 = until3 - from3
		val size2 = until2 - from2
		val size1 = until1 - from1
		val size  = (size3 * length2 + size2) * length1 + size1
		if (size < 0) {
			val real = (size3.toLong * length2 + size2) * length1 + size1
			throw new ArithmeticException(
				"Overflow when calculating the number of elements between (" + from3 + ", " + from2 + ", " + from1 +
					") and (" + until3 + ", " + until2 + ", " + until1 + ") in cube " +
					lengthOf(source) + " x " + length2 + " x " + length1 + ": " + size + " != " + real + "."
			)
		}
		make(source, from3, from2, from1, size)
	}

	protected def make[E](source :S[S[S[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E]

	protected override def make[T](source :S[S[S[T]]] @uncheckedVariance, first :Int, size :Int) :C[T] =
		if (size <= 0)
			empty(source)
		else lengthOf(source) match {
			case 0 => empty(source)
			case _ =>
				val a2 = get(source, 0)
				lengthOf(a2) match {
					case 0       => empty(source)
					case length2 => lengthOf(get(a2, 0)) match {
						case 0       => empty(source)
						case length1 =>
							make(length2, length1, source, first, size)
					}
				}
		}
	protected def make[T](length2 :Int, length1 :Int, source :S[S[S[T]]] @uncheckedVariance, first :Int, size :Int)
			:C[T] =
	{
		val length21 = length2.toLong * length1
		val from3   = first / length21
		val rem2    = first - from3 * length21
		val from2   = rem2 / length1
		val from1   = rem2 - from2 * length1
		make(source, from3.toInt, from2.toInt, from1.toInt, size)
	}


	protected def empty[E](source :S[S[S[E]]] @uncheckedVariance) :C[E] = Empty
	protected def empty[E] :C[E] = Empty

	protected val Empty :C[Nothing]

	protected final override def totalSizeOf[E](source :S[S[S[E]]] @uncheckedVariance) :Int =
		lengthOf(source) match {
			case    0 => 0
			case len3 =>
				val a2   = get(source, 0)
				lengthOf(a2) match {
					case    0 => 0
					case len2 => lengthOf(get(a2, 0)) * len2 * len3
				}
		}

	//Consider: replacing these with LikeIndexedSeq.Generic[S]
	protected def lengthOf[E](source :S[E]) :Int
	protected def get[E](source :S[E], index :Int) :E

}




trait ReverseCuboidSliceFactory[-S[_], +C[+_]]
	extends CuboidSliceFactory[S, C] with ReverseSizedSliceFactory[Pow[S]#_3 @uncheckedVariance, C]
{
	override def apply[E](source :S[S[S[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] =
		if (size <= 0 | from3 < 0 | from3 == 0 & (from2 < 0 | from2 == 0 & from1 < 0))
			empty(source)
		else
			super.apply(source, from3, from2, from1, size)

	override def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance, from :Int, size :Int) :C[E] =
		if (from < 0)
			empty(source)
		else
			super.apply(length2, length1, source, from, size)


	protected override def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                                from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] =
	{
		val length3 = lengthOf(source)
		val available = (from3.toLong * length2 + from2) * length1 + from1 + 1
		val size0 = math.min(available, size).toInt
		if (size0 == 0)
			empty(source)
		else if (from3 >= length3) {
			val size0 = math.min(length3 * length2 * length1.toLong, size).toInt //Recalculate just to be safe.
			make(source, length3 - 1, length2 - 1, length1 - 1, size0)
		} else
			make(source, from3, from2, from1, size0)
	}

	protected override def make[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                               from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val size = ((until3 - from3) * length2 + until2 - from2) * length1 + until1 - from1
		make(source, until3, until2, until1 - 1, size)
	}
}




trait AbstractCyclicCuboidSliceFactory[-S[_], +C[+_]] extends CuboidSliceFactory[S, C] {
	override def slice[E](source :S[S[S[E]]] @uncheckedVariance,
	                      from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val length3 = lengthOf(source)
		if (length3 == 0)
			return empty(source)
		var start3  = from3 % length3
		if (start3 < 0)
			start3 += length3
		var start2  = from2
		var start1  = from1
		var end3    = until3 % length3
		if (end3 < 0)
			end3 += length3
		var end2    = until2
		var end1    = until1
		var deltaFrom3  = if (from2 < 0 | from2 == 0 & from1 < 0) -1 else 0
		var deltaUntil3 = if (until2 < 0 | until2 == 0 & until1 <= 0) -1 else 0
		if (deltaFrom3 == -1)
			start3 = if (start3 == 0) length3 - 1 else start3 - 1
		if (deltaUntil3 == -1)
			end3 = if (end3 == 0) length3 - 1 else end3 - 1
		var a2 = get(source, end3)
		if (a2 == null)
			a2 = get(source, (end3 + 1) % length3)
		val length2 = lengthOf(a2)
		if (length2 == 0)
			return empty(source)

		if (deltaUntil3 == -1)
			end2 = length2 - 1
		else if (end1 <= 0) {
			end2 = math.min(length2, end2) - 1
			if (end1 == 0)
				end1 = Int.MaxValue
		} else if (end2 >= length2) {
			end2 = length2 - 1
			end1 = Int.MaxValue
		}
		if (deltaFrom3 == -1)
			start2 = length2 - 1
		else if (start1 < 0)
			start2 = math.min(length2, start2) - 1
		else if (start2 >= length2) {
			deltaFrom3 = 1
			start3 = (start3 + 1) % length3
			start2 = 0
			start1 = 0
		}
		var a1 = get(a2, end2)
		if (a1 == null)
			a1 = get(a2, (end2 + 1) % length2)
		val length1 = lengthOf(a1)
		if (length1 == 0)
			return empty(source)

		if (end1 < 0 | until2 < 0)
			end1 = length1 - 1
		else if (end1 == 0 | end1 > length1)
			end1 = length1
		if (start1 < 0 | from2 < 0)
			start1 = length1 - 1
		else if (start1 >= length1) {
			start1  = 0
			start2 += 1
			if (start2 == length2) {
				start2 = 0
				start3 = (start3 + 1) % length3
				deltaFrom3 += 1
			}
		}
		//Note that we are using from3 and until3, because we want a full iterator if they are unequal,
		// but equal mod length3 (after adjustments).
		val from0 = ((from3.toLong + deltaFrom3) * length2 + start2) * length1 + start1
		val until0 = ((until3.toLong + deltaUntil3) * length2 + end2) * length1 + end1
		if (from0 == until0)
			return empty(source)
		make(length2, length1, source, start3, start2, start1, end3, end2, end1)
	}

	override def slice[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance, from :Int, until :Int)
			:C[E] =
	{
		val length3  = lengthOf(source)
		val length21 = length2 * length1.toLong
		val total    = length3 * length21
		if (from == until | total <= 0 | length2 <= 0)
			return empty(source)
		val from0   = if (from < 0) (total + from % total) % total else from % total
		val from3   = from0 / length21
		val from21  = from0 - from3 * length21
		val from2   = from21 / length1
		val from1   = from21 - from2 * length1
		val until0  = if (until < 0) (total + until % total) % total else until % total
		val until3  = until0 / length21
		val until21 = until0 - until3 * length21
		val until2  = until21 / length1
		val until1  = until21 - until2 * length1
		make(length2, length1, source, from3.toInt, from2.toInt, from1.toInt, until3.toInt, until2.toInt, until1.toInt)
	}

	override def apply[E](source :S[S[S[E]]] @uncheckedVariance, from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] = {
		val length3 = lengthOf(source)
		if (size <= 0 | length3 == 0)
			return empty(source)
		var start3  = from3 % length3
		if (start3 < 0)
			start3 += length3
		var start2  = from2
		var start1  = from1
		val decStart3 = from2 < 0 | from2 == 0 & from1 < 0
		if (decStart3)
			start3 = if (start3 == 0) length3 - 1 else start3 - 1
		var a2 = get(source, start3)
		if (a2 == null)
			a2 = get(source, (start3 + 1) % length3)
		val length2 = lengthOf(a2)
		if (length2 == 0)
			return empty(source)

		if (decStart3)
			start2 = length2 - 1
		else if (start1 < 0)
			start2 = math.min(length2, start2) - 1
		else if (start2 >= length2) {
			start3 = (start3 + 1) % length3
			start2 = 0
			start1 = 0
		}
		var a1 = get(a2, start2)
		if (a1 == null)
			a1 = get(a2, (start2 + 1) % length2)
		val length1 = lengthOf(a1)
		if (length1 == 0)
			return empty(source)

		if (from2 < 0 | start1 < 0)
			start1 = length1 - 1
		else if (start1 >= length1) {
			start1  = 0
			start2 += 1
			if (start2 == length2) {
				start2 = 0
				start3 = (start3 + 1) % length3
			}
		}
		make(source, start3, start2, start1, math.min(size, length3.toLong * length2 * length1).toInt)
	}


	override def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance, from :Int, size :Int)
			:C[E] =
	{
		val length3  = lengthOf(source)
		val length21 = length2 * length1.toLong
		val total = length3 * length21
		if (size <= 0 | total <= 0 | length1 <= 0) //For total > 0 both length2 and length1 have to have the same sign.
			return empty(source)
		val from0 = {
			val mod = from % total
			if (mod < 0) total + mod else mod
		}
		val from3 = from0 / length21
		val rem3  = from0 - from3 * length21
		val from2 = rem3 / length1
		val from1 = rem3 - from2 * length1
		make(source, from3.toInt, from2.toInt, from1.toInt, math.min(total, size).toInt)
	}

	protected override def apply[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                                from3 :Int, from2 :Int, from1 :Int, size :Int) :C[E] =
		make(source, from3, from2, from1, math.min(size, lengthOf(source).toLong * length2 * length1).toInt)

}




trait CyclicCuboidSliceFactory[-S[_], +C[+_]]
	extends AbstractCyclicCuboidSliceFactory[S, C] with CyclicSliceFactory[Pow[S]#_3 @uncheckedVariance, C]
{
	protected override def make[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                               from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val from  = (from3.toLong * length2 + from2) * length1 + from1
		val until = (until3.toLong * length2 + until2) * length1 + until1
		val size0 =
			if (from < until) until - from
			else lengthOf(source).toLong * length2 * length1 + until - from //In case of equality take everything.
		val size = size0.toInt
		if (size < 0) {
			throw new ArithmeticException(
				"Overflow when calculating the number of elements between (" + from3 + ", " + from2 + ", " + from1 +
					") and (" + until3 + ", " + until2 + ", " + until1 + ") in cube " +
					lengthOf(source) + " x " + length2 + " x " + length1 + ": " + size + " != " + size0 + "."
			)
		}
		make(source, from3, from2, from1, size)
	}
}




trait ReverseCyclicCuboidSliceFactory[-S[_], +C[+_]]
	extends AbstractCyclicCuboidSliceFactory[S, C] with ReverseCyclicSliceFactory[Pow[S]#_3 @uncheckedVariance, C]
{
	protected override def make[E](length2 :Int, length1 :Int, source :S[S[S[E]]] @uncheckedVariance,
	                               from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) :C[E] =
	{
		val from  = (from3.toLong * length2 + from2) * length1 + from1
		val until = (until3.toLong * length2 + until2) * length1 + until1
		val size0 =
			if (from < until) until - from
			else lengthOf(source).toLong * length2 * length1 + until - from //In case of equality take everything.
		val size = size0.toInt
		if (size < 0) {
			throw new ArithmeticException(
				"Overflow when calculating the number of elements between (" + from3 + ", " + from2 + ", " + from1 +
					") and (" + until3 + ", " + until2 + ", " + until1 + ") in cube " +
					lengthOf(source) + " x " + length2 + " x " + length1 + ": " + size + " != " + size0 + "."
			)
		}
		if (until1 > 0)
			make(source, until3, until2, until1 - 1, size)
		else if (until2 == 0) {
			val adjusted3 = if (until3 == 0) lengthOf(source) - 1 else until3 - 1
			make(source, adjusted3, length2 - 1, length1 - 1, size)
		} else
			make(source, until3, until2 - 1, length1 - 1, size)
	}
}
