package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.nowarn
import scala.collection.{IterableFactory, IterableOps, StrictOptimizedIterableOps, mutable}
import scala.collection.mutable.Builder
import scala.util.Random

import net.noresttherein.sugar.arrays.{IArray, IRefArray}
import net.noresttherein.sugar.collections.extensions.SeqExtension
import net.noresttherein.sugar.exceptions.{illegal_!, outOfBounds_!}
import net.noresttherein.sugar.funny.generic.Any1




//consider: reversing the encoding to:
//  * The `n`-th element is the index of an element in an input sequence which occurs at index `n` in the output sequence.
// The upside is that reordering is simply this.toIndexedSeq.map(input). The downside is that not indexed sequences
// require an intermediate structure, which the current implementation does not. On the second hand, with reflection,
// in this scheme we could recreate the same collection type.
/** A permutation of values `[0, this.length)`, which can be used to reorder sequences of the same length.
  * The `n`-th element is the index in the output sequence of the `n`-th element from the input sequence.
  * Note that this is the inverse of the common notation for permutations, which lists indices of elements
  * from an input sequence in the order in which they should appear in the output.
  * @define Coll `Permutation`
  * @define coll permutation
  */
class Permutation private (override val toIndexedSeq :IndexedSeq[Int])
	extends AnyVal with IterableOnce[Int] with StrictOptimizedIterableOps[Int, IndexedSeq, IndexedSeq[Int]]
{
	/** The index in the output sequence to which the `n`-th element of an input sequence is mapped. */
	def apply(n :Int) :Int = toIndexedSeq(n)

	override def knownSize :Int = toIndexedSeq.length
	override def size :Int = toIndexedSeq.length
	def length :Int = toIndexedSeq.length

	def isIdentity :Boolean = toIndexedSeq == toIndexedSeq.indices

/*
	def apply[T, CC[_], C](items :collection.SeqOps[T, CC, C]) :C =
		if (items.size != toIndexedSeq.length)
			illegal_!(
				"Cannot reorder " + items + " according to " + this + " because its length " +
					items.size + " is different than " + length + "."
			)
		else if (items.sizeIs <= 1)
			items.drop(0)
		else {
			val indexed = items match {
				case seq :collection.IndexedSeq[T] @unchecked => seq
				case ops :collection.IndexedSeqOps[T, CC, C] @unchecked => ops(_:Int)
				case seq :collection.Seq[T] if seq.sizeIs <= 3 => seq
				case _ => items.toIndexedSeq //consider: MatrixBuffer
			}
			util.fromSpecific(items)(toIndexedSeq.view.map(indexed))
		}
*/

	//todo: use SeqLike type class
	/** Reorders the elements of the given collection according to this permutation.
	  * @return a sequence `s` of the same length as `items`, such that `s(this(i)) = items(i)`.
	  */
	@throws[IllegalArgumentException]("if items.length != this.length.")
	def apply[T](items :Seq[T]) :Seq[T] =
		if (items.length != toIndexedSeq.length)
			illegal_!(
				"Cannot reorder " + items + " according to " + this + " because its length " +
					items.length + " is different than " + length + "."
			)
		else if (items.sizeIs <= 1)
			items
		else { //todo: use MatrixBuffer
//		else if (toIndexedSeq.length <= ReasonableArraySize) {
			IRefArray.init[T](toIndexedSeq.length) { array =>
				val it = items.iterator
				var i = 0
				while (it.hasNext) {
					array(toIndexedSeq(i)) = it.next()
					i += 1
				}
			}.toIndexedSeq
		}

	/** The inverse to this permutation, that is a `Permutation` `p` such that `(p compose this) == Permutation.id`
	  * and `(this compose p) == Permutation.id`. Note that `seq.map(this.inverse) == this.reorder(seq)`.
	  */
	def inverse :Permutation = {
		val size = toIndexedSeq.length
		val seq = IArray.init[Int](size) { array =>
			var i = size
			while (i > 0) {
				i -= 1
				array(toIndexedSeq(i)) = i
			}
		}.toIndexedSeq
		new Permutation(seq)
	}

	/** Swaps the values at positions `n` and `m`. */
	def transpose(n :Int, m :Int) :Permutation =
		if (m >= toIndexedSeq.length || n >= toIndexedSeq.length)
			outOfBounds_!(
				"Permutation[" + toIndexedSeq.length + "].transpose(" + n + ", " + m + ")"
			)
		else
			new Permutation(
				RelayArray.tabulate(toIndexedSeq.length) { i =>
					if (i == n) toIndexedSeq(m) else if (i == m) toIndexedSeq(n) else toIndexedSeq(i)
				}
			)

	/** Splits this permutation into a sequence of permutations such that `split.reduce(_ ++ _) == this`.
	  * The underlying index range is first split into minimal, non empty sub ranges `[i, j)` such that
	  * `(i until j).forall(n => this(n) >= i && this(n) < j)`. Each range begets a `Permutation` by shifting down
	  * the indices in its domain and image by `i` - the first index in range.
	  */
	def split :Seq[Permutation] =
		if (toIndexedSeq.length == 0)
			RelayArray.one(this)
		else {
			val res = RelayArray.newBuilder[Permutation]
			var first = true
			val len = toIndexedSeq.length
			var limit = toIndexedSeq.head
			var start = 0
			while (limit < len) {
				var i = start
				while (i < limit) {
					limit = math.max(limit, toIndexedSeq(i))
					i += 1
				}
				if (first)
					res += new Permutation(toIndexedSeq.take(limit))
				else
					res += new Permutation(toIndexedSeq.view.slice(start, limit).map(_ - start).toIndexedSeq)
				first = false
				start += 1
				limit = toIndexedSeq(start)
			}
			res.result()
		}

	/** An orbit of a permutation element is the sequence of positions to which that element is mapped by recursively
	  * applying this permutation to itself:
	  * {{{
	  *     IndexedSeq.generate(n) { case i if this(i) != n => this(i) }
	  * }}}
	  */
	def orbit(n :Int) :IndexedSeq[Int] = {
		val res = RelayArray.newBuilder[Int]
		res += n
		var i = toIndexedSeq(n)
		while (i != n) {
			res += i
			i = toIndexedSeq(i)
		}
		res.result()
	}

	/** Lists all the cycles in this permutation. Each element of the sequence
	  * is an [[net.noresttherein.sugar.collections.Permutation.orbit orbit]] of some element not present
	  * in the preceding orbits.
	  */
	def cycles :Seq[IndexedSeq[Int]] = {
		val used = mutable.Set.empty[Int]
		val res = RelayArray.newBuilder[IndexedSeq[Int]]
		var last = 0
		while (toIndexedSeq.indexWhere(!used(_), last) match {
			case -1 => false
			case n  => res += orbit(n); last = n + 1; true
		}) {}
		res.result()
	}

	/** A permutation which applies first this permutation, then the argument. */
	def andThen(second :Permutation) :Permutation = second compose this

	/** A permutation `p` such that `p(i) == this(first(i))`. */
	def compose(first :Permutation) :Permutation =
		if (first.length != length)
			illegal_!(
				"Cannot compose permutations of different lengths: " + this + " compose " + first + "."
			)
		else
			new Permutation(first.toIndexedSeq.map(toIndexedSeq))

	/** A concatenation of two permutations. The second permutation is used to map indices
	  * `[this.length, this.length + other.length)` to the same range. */
	@inline final def ++(other :Permutation) :Permutation = concat(other)

	/** A concatenation of two permutations. The second permutation is used to map indices
	  * `[this.length, this.length + other.length)` to the same range. */
	def concat(other :Permutation) :Permutation = {
		val len = toIndexedSeq.length
		new Permutation(toIndexedSeq :++ other.toIndexedSeq.map(_ + len))
	}

	@nowarn
	override def toIterable :Iterable[Int] = toIndexedSeq
	override def toSeq :Seq[Int] = toIndexedSeq
	override def iterator :Iterator[Int] = toIndexedSeq.iterator
	protected override def coll :IndexedSeq[Int] = toIndexedSeq

	override def iterableFactory :IterableFactory[IndexedSeq] = IndexedSeq

	protected override def fromSpecific(coll :IterableOnce[Int]) :IndexedSeq[Int] =
		coll match {
			case items :Iterable[Int] => items.toIndexedSeq
			case _ => coll.iterator.toIndexedSeq
		}
	protected override def newSpecificBuilder :Builder[Int, IndexedSeq[Int]] = IndexedSeq.newBuilder

	override def toString :String = toIndexedSeq.mkString("Permutation(", ", ", ")")
}




/**
  * @define Coll `Permutation`
  * @define coll permutation
  */
object Permutation {
	/** An empty permutation. */
	val empty = new Permutation(IndexedSeq.empty)

	/** The only permutation for sequences of length `1`. */
	val singleton = new Permutation(RelayArray.one(0))

	/** An identity permutation `0 until size`. */
	def id(size :Int) :Permutation = new Permutation(0 until size)

	/** A permutation reversing the order of elements in any sequence of length `size`. */
	def reversed(size :Int) :Permutation = new Permutation(size - 1 to 0 by -1)

	/** Wraps the given sequence of non repeating elements `[0, n]` in a `Permutation`. */
	@throws[IllegalArgumentException]("if indices is not a valid permutation.")
	def fromSeq(indices :IndexedSeq[Int]) :Permutation =
		if (indices.isEmpty)
			new Permutation(indices)
		else if (indices.sorted != indices.indices)
			illegal_!(
				indices.toString + " is not a valid permutation."
			)
		else if (indices.isInstanceOf[StrictOptimizedIterableOps[_, Any1, _]])
			new Permutation(indices)
		else
			new Permutation(indices to DefaultArraySeq)

	/** A permutation `p` such that `p.`[[net.noresttherein.sugar.collections.Permutation.apply apply]](`from) == to`. */
	@throws[IllegalArgumentException]("if from or to contains duplicates, or if from.toSet != to.toSet.")
	def between[T](from :Seq[T], to :Seq[T]) :Permutation = {
		val in = from to Ranking
		val out = to to Ranking
		if (in.length != from.length)
			illegal_!("Input sequence contains duplicates: " + from + ".")
		if (out.length != to.length)
			illegal_!("Output sequence contains duplicates: " + to + ".")
		if (in.length != out.length)
			illegal_!(
				from.toString  + " and " + to + " are of different lengths: " + from.length + " vs. " + to.length
			)
		if (!in.forall(out.toSet))
			illegal_!(
				from.toString + " and " + to + " contain different elements: " + in.find(!out.contains(_)).get +
					" does not exist in the output sequence."
			)
		val size = in.size
		val res = new Array[Int](size)
		var i = 0
		while (i < size) {
//			res(i) = in.indexOf(out(i))
			res(i) = out.indexOf(in(i))
			i += 1
		}
		new Permutation(IArray.Wrapped(res.asInstanceOf[IArray[Int]]))
	}

	/** A random permutation of size `size` with equal distribution. */
	def random(size :Int)(implicit random :Random) :Permutation = new Permutation((0 until size).shuffled)
}
