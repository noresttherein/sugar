package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, BufferedIterator, View, mutable}

import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, IRefArray, MutableArrayExtension}
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.extensions.IteratorExtension
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.numeric.extensions.BooleanExtension
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.exceptions.{noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.One




//todo: Views with the same functionality
//todo: drop/take/copyToArray methods are untested!
/** Iterators implementing various extension methods from `extensions`. */
private object Iterators {

	//shared implementations of extensions methods from IterableExtension and IteratorExtension

	def single[E](elem :E) :Iterator[E] = new Single(elem)

	def double[E](first :E, second :E) :Iterator[E] = new Double(first, second)

	def const[E](value :E, length :Int) :Iterator[E] = new Const(length, value)

	def const[E](value :E) :Iterator[E] = new ConstInfinite(value)

	def scanLeft[E, A](self :Iterator[E], z :A, f :(A, E) => A) :Iterator[A] =
		new ScanLeft(self, z, f)


	def zipEven[A, B](left :Iterator[A], right :Iterator[B]) :Iterator[(A, B)] = new ZipEven(left, right)

	def zipMap[A, B, O](left :Iterator[A], right :Iterator[B])(f :(A, B) => O) :Iterator[O] =
		left.zip(right).map(f.tupled)

	def zipMapEven[A, B, O](left :Iterator[A], right :Iterator[B])(f :(A, B) => O) :Iterator[O] =
		new ZipEven(left, right).map(f.tupled)

	def zipMapAll[A, B, O](left :Iterator[A], right :Iterator[B], leftElem :A, rightElem :B)
	                      (f :(A, B) => O) :Iterator[O] =
		left.zipAll(right, leftElem, rightElem).map(f.tupled)

	def zipFlatMap[A, B, O](left :Iterator[A], right :Iterator[B])(f :(A, B) => IterableOnce[O]) :Iterator[O] =
		left.zip(right).flatMap(f.tupled)

	def zipFlatMapEven[A, B, O](left :Iterator[A], right :Iterator[B])(f :(A, B) => IterableOnce[O]) :Iterator[O] =
		new ZipEven(left, right).flatMap(f.tupled)

	def zipFlatMapAll[A, B, O](left :Iterator[A], right :Iterator[B], leftElem :A, rightElem :B)
	                          (f :(A, B) => IterableOnce[O]) :Iterator[O] =
		left.zipAll(right, leftElem, rightElem).flatMap(f.tupled)

	def zip3[A, B, C](first :Iterator[A], second :Iterator[B], third :Iterator[C]) :Iterator[(A, B, C)] =
		new Zip3(first, second, third)

	def zipEven3[A, B, C](first :Iterator[A], second :Iterator[B], third :Iterator[C]) :Iterator[(A, B, C)] =
		new ZipEven3(first, second, third)

	def zipAll3[A, B, C](first :Iterator[A], second :Iterator[B], third :Iterator[C],
	                     firstElem :A, secondElem :B, thirdElem :C) :Iterator[(A, B, C)] =
		new ZipAll3(first, firstElem, second, secondElem, third, thirdElem)

	def zipTail[A](self :Iterator[A]) :Iterator[(A, A)] =
		if (self.isEmpty) Iterator.empty
		else new ZipTail(self)


	def mapWith[E, O, A](self :Iterator[E], z :A, f :(E, A) => (O, A)) :Iterator[O] =
		new MapWith(self, z, f)

	def flatMapWith[E, O, A](self :Iterator[E], z :A, f :(E, A) => (IterableOnce[O], A)) :Iterator[O] =
		mapWith(self, z, f).flatten

	def mapWithIndex[E, O](self :Iterator[E], f :(E, Int) => O) :Iterator[O] = new MapWithIndex(self, f)

	def flatMapWithIndex[E, O](self :Iterator[E], f :(E, Int) => IterableOnce[O]) :Iterator[O] =
		mapWithIndex(self, f).flatten

	def collectWithIndex[E, O](self :Iterator[E], f :PartialFunction[(E, Int), O]) :Iterator[O] =
		new CollectWithIndex(self, f)

	def mapWhile[E, O, A](self :Iterator[E], z :A, pred :A => Boolean, f :(A, E) => (A, O)) :Iterator[O] =
		new MapWhile(self, z, pred, f)

	def flatMapWhile[E, O, A](self :Iterator[E], z :A, pred :A => Boolean, f :(A, E) => (A, IterableOnce[O]))
			:Iterator[O] =
		mapWhile(self, z, pred, f).flatten

	def mapUntil[E, A, O](self :Iterator[E], z :A, f :(A, E) => (Boolean, A, O)) :Iterator[O] =
		new MapUntil(self, z, f)

	def flatMapUntil[E, A, O](self :Iterator[E], z :A, f :(A, E) => (Boolean, A, IterableOnce[O])) :Iterator[O] =
		mapUntil(self, z, f).flatten

	def mapSome[E, A, O](self :Iterator[E], z :A, f :(A, E) => Option[(A, O)]) :Iterator[O] =
		new MapSome(self, z, f)

	def flatMapSome[E, A, O](self :Iterator[E], z :A, f :(A, E) => Option[(A, IterableOnce[O])]) :Iterator[O] =
		mapSome(self, z, f).flatten

	def mapPrefix[E, A, O](self :Iterator[E], z :A, f :(A, E) => Opt[(A, O)]) :Iterator[O] =
		new MapPrefix(self, z, f)

	def flatMapPrefix[E, A, O](self :Iterator[E], z :A, f :(A, E) => Opt[(A, IterableOnce[O])]) :Iterator[O] =
		mapPrefix(self, z, f).flatten


	def filterWith[E, A](self :Iterator[E], z :A, pred :(E, A) => (Boolean, A), keep :Boolean = true) :Iterator[E] =
		new FilterWith(self, z, pred, keep)

	def filterWithIndex[A](self :Iterator[A], pred :(A, Int) => Boolean, keep :Boolean = true) :Iterator[A] =
		new FilterWithIndex(self, pred, keep)

	def keep[E](self :Iterator[E], pred :Int => Boolean) :Iterator[E] =
		if (self.knownSize == 0) self else new Keep(self, pred, true)

	def distinct[E](self :Iterator[E]) :Iterator[E] = {
		val size = self.knownSize
		if (size >= 0 && size <= 1) self else new FirstOccurrences(self)
	}


	//All the following iterators could be simply replaced with
	def removed[E](self :Iterator[E], index :Int) :Iterator[E] =
		if (index < 0 || { val s = self.knownSize; s >= 0 & index >= s })
			outOfBounds_!(index)
//		else if (index == 0) //breaks laziness
//			self.next()
		else
			new Removed(self, index)

	def removed[E](self :Iterator[E], from :Int, until :Int) :Iterator[E] =
		if (until <= 0 | until <= from)
			self
		else {
			val size        = self.knownSize
			val nonNegFrom  = math.max(from, 0)
			val nonNegUntil = math.max(until, from)
			if (size >= 0 && from >= size)
				self
			else
				new RemovedSlice(self, nonNegFrom, nonNegUntil)
		}


	def updated[E](self :Iterator[E], index :Int, elem :E) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index >= size })
			outOfBounds_!(self.toString + ".updated(" + index + ", _)")
		else
			new Updated(self, index, elem)

	def updatedAll[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] = {
		val size      = self.knownSize
		val patchSize = elems.knownSize
		if (index < 0 || size >= 0 & index > (if (patchSize >= 0) size - patchSize else size))
			outOfBounds_!(
				self.toString + (if (size >= 0) "|" + size + "|.updatedAll(" else ".updatedAll(") +
					index + ", " + elems + ")"
			)
		if (patchSize == 0 && size >= 0)
			self
		else
			new UpdatedAll(self, index, elems.iterator)
	}

	def overwritten[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] = {
		val size      = self.knownSize
		val patchSize = elems.knownSize
		if (patchSize == 0 || size == 0 || index <= 0 && patchSize >= 0 && index + patchSize <= 0 ||
			size >= 0 && index >= size
		)
			self
		else
			new Overwritten(self, index, elems.iterator)
	}

	//todo: permissive indexing. It is better to be consistent with patch,
	// and because the valid range in Ranking depends on whether the element is already in the collection.
	def inserted[E](self :Iterator[E], index :Int, elem :E) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index > size })
			outOfBounds_!(self.toString + ".insertedAll(" + index.toString + ", " + elem + ")")
		else
			new Inserted(self, index, elem)

	//todo: permissive indexing
	def insertedAll[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index > size })
			outOfBounds_!(self.toString + ".insertedAll(" + index.toString + ", _:*)")
		else
			new InsertedAll(self, index, elems.iterator)

	def appended[E](self :Iterator[E], elem :E) :Iterator[E] = concat(self, Iterator.single(elem))

	def prepended[E](self :Iterator[E], elem :E) :Iterator[E] = concat(Iterator.single(elem), self)

	def concat[E](first :Iterator[E], second :Iterator[E]) :Iterator[E] =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else first match {
			case i :Concat[E ]      => i.append(second)
			case i :IteratorPair[E] => i.append(second)
			case _                  => second match {
				case i :Concat[E]       => i.prepend(first)
				case i :IteratorPair[E] => i.prepend(first)
				case _                  => new IteratorPair(first, second)
			}
		}

	@tailrec def reverse[E](items :IterableOnce[E]) :Iterator[E] = items match {
		case indexed :collection.IndexedSeqOps[E, generic.Any1, _] =>
			if (indexed.isEmpty) Iterator.empty[E] else indexed.reverseIterator
		case ranking :Ranking[E]                        => ranking.reverseIterator
		case view    :View[E]                           => reverse(view.iterator)
		case it      :Iterable[E] if it.sizeIs <= 1     => it.iterator
		case it      :Iterator[E] if !it.hasNext        => it
		case IndexedIterable(seq)                       => seq.reverseIterator
		case sorted :collection.SortedSet[E @unchecked] =>
			new ReverseSortedSetIterator(sorted)
		case sorted :collection.SortedMap[_, _] =>
			new ReverseSortedMapIterator(sorted).asInstanceOf[Iterator[E]]
		case _ =>
			val size = items.knownSize
			if (size >= 0) (TemporaryBuffer.ofCapacity[E](size) ++= items).reverseIterator
			else (TemporaryBuffer.of[E] ++= items).reverseIterator
	}

	def takeRight[E](self :Iterator[E], n :Int) :Iterator[E] =
		if (n <= 0)
			Iterator.empty
		else {
			val size = self.knownSize
			if (size >= 0)
				if (size <= n) self
				else self.drop(size - n)
			else
				new TakeRight(self, n)
		}

	def takeUntil[A, E](self :Iterator[E], z :A, pred :A => Boolean, op :(A, E) => A) :Iterator[E] =
		if (self.knownSize == 0) self
		else if (pred(z)) Iterator.empty
		else new TakeUntil(self, z, pred, op)

	def takeWith[A, E](self :Iterator[E], z :A, pred :A => Boolean, op :(A, E) => A) :Iterator[E] =
		if (self.knownSize == 0) self else new TakeWith(self, z, pred, op)

	def dropUntil[A, E](self :Iterator[E], z :A, pred :A => Boolean, op :(A, E) => A) :Iterator[E] =
		if (self.knownSize == 0 || pred(z)) self
		else new DropUntil(self, z, pred, op)

	def dropWith[A, E](self :Iterator[E], z :A, pred :A => Boolean, op :(A, E) => A) :Iterator[E] =
		if (self.knownSize == 0) self else new DropWith(self, z, pred, op)

	def drop[E](itr :Iterator[E], n :Int, strict :Boolean) :Iterator[E] =
		if (n <= 0) itr
		else if (strict) HasFastSlice.drop(itr, n)
		else itr match {
			case sugared :SugaredIterator[E]                                => sugared.drop(n)
			case _ if ({ val size = itr.knownSize; size >= 0 & n >= size }) => itr
			case _                                                          => itr.drop(n)
		}

	def slice[E](itr :Iterator[E], from :Int, until :Int, strict :Boolean) :Iterator[E] =
		if (until <= 0 | until <= from)
			itr
		else if (strict)
			HasFastSlice.slice(itr, from, until)
		else itr match {
			case sugared :SugaredIterator[E] =>
				sugared.slice(from, until)
			case _ => //Sadly, default Iterator.slice doesn't check for corner cases.
				val size = itr.knownSize
				if (size >= 0)
					if (from >= size) Iterator.empty
					else if (from <= 0 & until >= size) itr
					else if (from <= 0) itr.take(until)
					else if (until <= size) itr.drop(from)
					else itr.slice(from, until)
				else
					if (from <= 0) itr.take(until)
					else itr.slice(from, until)
		}


	/** An iterator with safe slicing methods. Invoking `take`, `drop`, `slice` does not invalidate this iterator;
	  * instead, iterators returned by those methods share the same underlying state,
	  * including a counter of already returned elements.
	  * Calling `take` on this iterator returns a new iterator, which will not return elements past a certain index.
	  * This iterator remains unaffected by the call itself, or `take` called on the iterator it created,
	  * but advancing the latter - via `next` or `drop` - automatically also advances this iterator
	  * by the same number of elements, and vice versa.
	  *
	  * Likewise, `copyToArray` is guaranteed to advance this iterator - and all created by it -
	  * exactly by the number of written elements, as returned by the method.
	  * @example
	  * {{{
	  *     val iter   = source.iterator.safe
	  *     val arrays = Array.ofDim[Int](n, m)
	  *     var i = 0
	  *     while (iter.hasNext && i < n) {
	  *         iter.take(m).copyToArray(arrays(i))
	  *         i += 1
	  *     }
	  * }}}
	  */
	def slicer[E](self :Iterator[E]) :Iterator[E] =
		if (!self.hasNext) self else new Slicer(self)

	/** Same as `Iterator.splitAt`, but has efficient `drop`/`take` if the underlying iterator
	  * has efficient implementations of these methods.
	  */
	def splitAt[E](iter :Iterator[E], idx :Int) :(Iterator[E], Iterator[E]) = {
		val size = iter.knownSize
		if (idx <= 0)
			if (size == 0) splitEmpty else (Iterator.empty, iter)
		else if (idx >= size & size >= 0)
			(iter, Iterator.empty)
		else {
			val prefix = new Take(iter, idx)
			(prefix, prefix.suffix)
		}
	}

	val splitEmpty = (Iterator.empty, Iterator.empty)



	private[sugar] trait StrictIterator[+E] extends IteratorWithDrop[E] {
		override def hasFastDrop = true
		override def strictDrop(n :Int) :Iterator[E] = this.drop(n)
	}
	private[sugar] trait SlowDropIterator[+E] extends SugaredIterator[E] {
		override def hasFastDrop = false
		override def strictDrop(n :Int) :Iterator[E] = {
			var rem = n
			while (rem >= 0 && hasNext) {
				rem -= 1
				next()
			}
			this
		}
		override def strictSlice(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from)
				Iterator.empty
			else {
				val size = knownSize
				if (from <= 0 & size >= 0 & until >= size) this
				else if (from <= 0) take(until)
				else if (size >= 0 & until >= size) strictDrop(until)
				else strictDrop(from).take(until - from)
			}
	}
	//Consider: it is inconsistent that IteratorWithDrop is in collections, and this one is an inner class.
	private[sugar] trait IteratorWithTake[+E] extends SugaredIterator[E] {
		override def strictDrop(n :Int) :Iterator[E] = drop(n, true)
		override def drop(n :Int) :Iterator[E] = drop(n, false)
		protected def drop(n :Int, strict :Boolean) :Iterator[E] = {
			val res = super.drop(n)
			if (strict)
				res.hasNext
			res
		}
		override def slice(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from) Iterator.empty[E]
			else if (from <= 0) take(until)
			else take(until).drop(from)

		override def strictSlice(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from) Iterator.empty
			else if (from <= 0) take(until)
			else take(until).strictDrop(from)
	}
	//todo: a better name for SingleDrop.
	private trait SingleDrop[+E] extends IteratorWithDrop[E] {
		override def drop(n :Int) :Iterator[E] = drop(n, false)
		override def strictDrop(n :Int) :Iterator[E] = drop(n, true)
		protected def drop(n :Int, strict :Boolean) :Iterator[E]
	}

	private[sugar] abstract class IteratorKnownSize[+E](private[this] var currentSize :Int)
		extends AbstractIterator[E] with HasFastSlice[E]
	{
		final override def knownSize = currentSize
		protected final def knownSize_=(value :Int) :Unit = currentSize = value
		protected final def knownSize_--() :Unit = currentSize -= 1
		override def hasNext = currentSize > 0
	}
	private[sugar] abstract class IteratorTake[+E](size :Int)
		extends IteratorKnownSize[E](size) with IteratorWithTake[E]
	{
		override def take(n :Int) :Iterator[E] =
			if (n <= 0) {
				knownSize = 0; this
			} else if (knownSize >= n) {
				knownSize = n; this
			} else if (knownSize >= 0)
				this
			else
				super.take(n)
	}
	private[sugar] sealed abstract class IteratorDrop[+E](size :Int)
		extends IteratorKnownSize[E](size) with StrictIterator[E]
	{
//		override def hasFastDrop = true
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0)
				knownSize = math.max(0, knownSize - n)
			this
		}
	}


	private final class Single[+E](hd :E) extends IteratorTake[E](1) with StrictIterator[E] {
//		override def hasFastDrop = true
		override def next() :E =
			if (hasNext) hd else noSuch_!("Iterator.empty")

		override def drop(n :Int) :Iterator[E] =
			if (n > 0) super.take(0) else this

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = util.elementsToCopy(knownSize, xs, start, len)
			if (copied > 0)
				xs(start) = hd
			copied
		}
		override def toString :String = if (hasNext) "Iterator(" + hd + ")" else "Iterator()"
	}

	private final class Double[+E](first :E, second :E) extends IteratorDrop[E](2) {
		override def next() :E = knownSize match {
			case 2 => knownSize = 1; first
			case 1 => knownSize = 0; second
			case _ => noSuch_!("Iterator.empty")
		}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else {
				val size = knownSize
				if (n >= size) this
				else Iterator.single(first)
			}
		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
			val size = knownSize
			val copied = util.elementsToCopy(size, xs, start, len)
			if (copied > 0)
				if (copied == 2) {
					xs(start) = first
					xs(start + 1) = second
				} else if (size == 2)
					xs(start) = first
				else
					xs(start) = second
			knownSize -= copied
			copied
		}
		override def toString :String = knownSize match {
			case 2 => "Iterator(" + first + ", " + second + ")"
			case 1 => "Iterator(" + second + ")"
			case _ => "Iterator()"
		}
	}

	private final class ConstInfinite[+E](override val head :E)
		extends AbstractIterator[E] with BufferedIterator[E] with StrictIterator[E] with HasFastSlice[E]
	{
//		override def hasFastDrop = true
		override def hasNext :Boolean = true
		override def next() :E = head
		override def drop(n :Int) :Iterator[E] = this
		override def take(n :Int) :Iterator[E] = if (n <= 0) Iterator.empty else new Const(n, head)
		override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
			if (n <= 0) (Iterator.empty, this)
			else (new Const(n, head), this)

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 || start >= xs.length) {
				val copied = math.min(len, xs.length - start)
				xs.fill(start, start + copied)(head)
				copied
			} else
				0
		override def toString :String = "Iterator.infinite(" + head + "*)"
	}

	private final class Const[+E](initSize :Int, override val head :E)
		extends IteratorTake[E](initSize) with BufferedIterator[E] with StrictIterator[E]
	{
		override def next() :E = {
			val size = knownSize
			if (size <= 0)
				noSuch_!(toString)
			knownSize -= 1; head
		}
//		override def hasFastDrop = true
		override def drop(n :Int) :Iterator[E] = take(knownSize - n)

		override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
			if (knownSize == 0) splitEmpty
			else if (n <= 0) (Iterator.empty, this)
			else if (n >= knownSize) (this, Iterator.empty)
			else (new Const(n, head), drop(n))

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
			val copied = util.elementsToCopy(knownSize, xs, start, len)
			xs.fill(start, start + copied)(head)
			copied
		}
		override def toString :String = "Iterator.const|" + knownSize + "|(" + head + ")"
	}


	/** Base traits for iterators mapping other iterators. This doesn't necessary mean `map`,
	  * just that the size of the extending iterator equals the size of the underlying iterator,
	  * and that the elements of this iterator can be dropped by simply dropping the elements
	  * in the underlying iterator.
	  */
	private[collections] abstract class AbstractMap[X, +Y](private[this] var underlying :Iterator[X])
		extends SugaredIterator[Y]
	{
		protected def source :Iterator[X] = underlying
		override def hasFastDrop = HasFastSlice(underlying)
		override def knownSize :Int = underlying.knownSize
		override def hasNext :Boolean = underlying.hasNext
		override def take(n :Int) :Iterator[Y] =
			if (n <= 0)
				Iterator.empty
			else {
				val size = underlying.knownSize
				if (size == -1 | n < size)
					underlying = underlying.take(n)
				this
			}
		protected def drop(n :Int, strict :Boolean) :Iterator[Y] = {
			underlying = Iterators.drop(underlying, n, strict)
			this
		}
		override def drop(n :Int) :Iterator[Y] = drop(n, false)
		override def strictDrop(n :Int) :Iterator[Y] = drop(n, true)

		protected def slice(from :Int, until :Int, strict :Boolean) :Iterator[Y] =
			if (until <= 0 | until <= from)
				Iterator.empty
			else {
				underlying = Iterators.slice(underlying, from, until, strict)
				this
			}
		override def slice(from :Int, until :Int) :Iterator[Y] = slice(from, until, false)
		override def strictSlice(from :Int, until :Int) :Iterator[Y] = slice(from, until, true)
	}

	private abstract class AbstractFlatMap[+Y] extends AbstractIterator[Y] with IteratorWithDrop[Y] {
		private[this] var curr :Iterator[Y] = _
		private[this] var currNonEmpty = false
		def source :Iterator[Y] = curr

		protected def enqueue(items :Iterator[Y @uncheckedVariance]) :Boolean = {
			curr = items
			currNonEmpty = curr.hasNext
			currNonEmpty
		}

		override def hasNext :Boolean = currNonEmpty
		override def next() :Y = {
			if (!currNonEmpty && !hasNext)
				noSuch_!("empty iterator")
			val res = curr.next()
			currNonEmpty = curr.hasNext
			res
		}
	}


	private class MapWith[X, A, +Y](private[this] var source :Iterator[X],
	                                private[this] var acc :A, f :(X, A) => (Y, A))
		extends AbstractIterator[Y] with SlowDropIterator[Y]
	{
		override def knownSize :Int = source.knownSize

		override def hasNext :Boolean = source.hasNext
		override def next() :Y = { val res = f(source.next(), acc); acc = res._2; res._1 }

		override def toString :String = source.toString + ".mapWith(" + acc + ")"
	}

	private class ScanLeft[X, +Y](private[this] var souce :Iterator[X], private[this] var acc :Y, f :(Y, X) => Y)
		extends AbstractBufferedIterator[Y] with SlowDropIterator[Y]
	{
		push(acc)
		override def knownSize :Int = {
			val size = souce.knownSize
			if (size < 0) -1 else size + super.hasNext.toInt
		}
		override def hasNext :Boolean = super.hasNext || souce.hasNext && {
			acc = f(acc, souce.next())
			push(acc)
		}
		override def toString :String = souce.toString + ".scanLeft(->" + acc + ")"
	}


	private final class MapWithIndex[X, +Y](underlying :Iterator[X], f :(X, Int) => Y)
		extends AbstractMap[X, Y](underlying)
	{
		private[this] var i :Int = -1
		override def next() :Y = { i += 1; f(source.next(), i) }

		override def strictDrop(n :Int) = { i += math.max(n, 0); super.strictDrop(n) }
		override def drop(n :Int) = { i += math.max(n, 0); super.drop(n) }
		override def slice(from :Int, until :Int) = { i += math.max(from, 0); super.slice(from, until) }

		override def toString :String = source.toString + ".mapWithIndex(@" + i + ")"
	}

	private final class CollectWithIndex[X, Y](private[this] var source: Iterator[X], f: PartialFunction[(X, Int), Y])
		extends AbstractBufferedIterator[Y] with SlowDropIterator[Y]
	{
		private[this] val fallback = Iterators.fallback.asInstanceOf[((X, Int)) => Y]
		private[this] var idx = -1
		override def hasNext :Boolean = super.hasNext || {
			var found = false
			while(!found && source.hasNext) {
				idx += 1
				val next = f.applyOrElse((source.next(), idx), fallback)
				if (next.asAnyRef ne fallback) {
					push(next)
					found = true
				}
			}
			found
		}
		override def toString :String = source.toString + ".collectWithIndex@" + idx + "(" + f + ")"
	}
	private val fallback :Any => Any = _ => fallback


	private final class MapWhile[X, S, +Y](source :Iterator[X], private[this] var state :S,
	                               pred :S => Boolean, f :(S, X) => (S, Y))
		extends AbstractIterator[Y] with SlowDropIterator[Y]
	{
		override def hasNext :Boolean = source.hasNext && pred(state)
		override def next() :Y = {
			val (a, res) = f(state, source.next())
			state = a
			res
		}
		override def toString :String = source.toString + ".mapWhile(" + state + ")"
	}

	private final class MapUntil[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => (Boolean, S, Y))
		extends AbstractBufferedIterator[Y] with SlowDropIterator[Y]
	{
		override def hasNext :Boolean = super.hasNext || source.hasNext && {
			val (cont, s, y) = f(state, source.next())
			state = s
			if (!cont)
				push(y)
			!cont
		}
		override def toString :String = source.toString + ".mapUntil(" + state + ")"
	}

	private final class MapSome[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => Option[(S, Y)])
		extends AbstractIterator[Y] with SlowDropIterator[Y]
	{
		private[this] var hd :Option[(S, Y)] = _
		override def hasNext :Boolean = {
			if (hd eq null)
				hd = if (source.hasNext) f(state, source.next()) else None
			hd.isDefined
		}
		override def next() :Y = {
			if (hd eq null)
				hd = f(state, source.next())
			val res = hd.get
			hd = null
			state = res._1
			res._2
		}
		override def toString :String = hd match {
			case null         => source.toString + ".mapSome(" + state + ")"
			case Some((s, e)) => source.toString + ".mapSome(" + s + "->" + e + ")"
			case _            => "Iterator()"
		}
	}

	private final class MapPrefix[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => Opt[(S, Y)])
		extends AbstractIterator[Y] with SlowDropIterator[Y]
	{
		private[this] var hd :Opt[(S, Y)] = this.asInstanceOf[Opt[(S, Y)]] //Opt is erased, we use this as a marker.
		override def hasNext :Boolean = {
			if (hd.asAnyRef eq this)
				hd = if (source.hasNext) f(state, source.next()) else None
			hd.isDefined
		}
		override def next() :Y = {
			if (hd.asAnyRef eq this)
				hd = f(state, source.next())
			val res = hd.get
			hd = null
			state = res._1
			res._2
		}
		override def toString :String = hd match {
			case _ if hd.asAnyRef eq this => source.toString + ".mapPrefix(" + state + ")"
			case One((s, e))              => source.toString + ".mapPrefix(" + s + "->" + e + ")"
			case _                        => "Iterator()"
		}
	}


	private final class FilterWith[E, S](underlying :Iterator[E], private[this] var state :S,
	                                     f :(E, S) => (Boolean, S), keep :Boolean = true)
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		override def hasNext :Boolean = super.hasNext || {
			while (underlying.hasNext) {
				val peek         = underlying.next()
				val (include, s) = f(peek, state)
				state = s
				if (include == keep) {
					return push(peek)
				}
			}
			false
		}
		override def toString :String =
			if (super.hasNext)
				"Iterator(" + head + ",...)"
			else {
				val name = if (keep) ".filterWith(" else "Iterator.filterNotWith("
				underlying.toString + name + state + ")"
			}
	}

	private final class FilterWithIndex[E](private[this] var source :Iterator[E],
	                                       f :(E, Int) => Boolean, keep :Boolean = true)
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		private[this] var i = -1
		override def hasNext :Boolean = super.hasNext || {
			while (source.hasNext) {
				val peek = source.next()
				i += 1
				if (f(peek, i) == keep) {
					return push(peek)
				}
			}
			false
		}
		override def toString :String =
			if (super.hasNext)
				"Iterator(" + head + ",...)"
			else {
				val name = if (keep) ".filterWithIndex" else ".filterNotWithIndex"
				source.toString + "@" + (i + 1) + name
			}
	}


	private abstract class AbstractZip3[A, B, C](final protected var itr1 :Iterator[A], final protected var itr2 :Iterator[B],
	                                             final protected var itr3 :Iterator[C])
		extends AbstractIterator[(A, B, C)] with SugaredIterator[(A, B, C)]
	{
		private[this] var continue1 = true
		private[this] var continue2 = true
		private[this] var continue3 = true
		protected final def hasNext1 = continue1 & { continue1 = itr1.hasNext; continue1 }
		protected final def hasNext2 = continue2 & { continue2 = itr2.hasNext; continue2 }
		protected final def hasNext3 = continue3 & { continue3 = itr3.hasNext; continue3 }

		override def hasFastDrop = HasFastSlice(itr1) && HasFastSlice(itr2) && HasFastSlice(itr3)

		override def strictDrop(n :Int) :Iterator[(A, B, C)] = drop(n, true)
		override def drop(n :Int)       :Iterator[(A, B, C)] = drop(n, false)

		private def drop(n :Int, strict :Boolean) :Iterator[(A, B, C)] =
			if (n > 0 & HasFastSlice.hasFastDrop(itr1) && HasFastSlice.hasFastDrop(itr2) && HasFastSlice.hasFastDrop(itr3)) {
				itr1 = Iterators.drop(itr1, n, strict)
				itr2 = Iterators.drop(itr2, n, strict)
				itr3 = Iterators.drop(itr3, n, strict)
				this
			} else if (strict)
				super.strictDrop(n)
			else
				super.drop(n)

		override def take(n :Int) :Iterator[(A, B, C)] =
			if (n <= 0)
				Iterator.empty
			else if ({ val size = knownSize; size >= 0 & n >= size})
				this
			else if (HasFastSlice.hasFastDrop(itr1) && HasFastSlice.hasFastDrop(itr2) && HasFastSlice.hasFastDrop(itr3)) {
				itr1 = itr1.take(n)
				itr2 = itr2.take(n)
				itr3 = itr3.take(n)
				this
			} else
				super.take(n)

		override def strictSlice(from :Int, until :Int) :Iterator[(A, B, C)] = slice(from, until, true)
		override def slice(from :Int, until :Int)       :Iterator[(A, B, C)] = slice(from, until, false)

		private def slice(from :Int, until :Int, strict :Boolean) :Iterator[(A, B, C)] =
			if (until <= 0 | until <= from)
				Iterator.empty
			else if (from <= 0)
				take(until)
			else if ({ val size = knownSize; size >= 0 & until >= size })
				drop(from)
			else if (HasFastSlice.hasFastDrop(itr1) && HasFastSlice.hasFastDrop(itr2) && HasFastSlice.hasFastDrop(itr3)) {
				itr1 = Iterators.slice(itr1, from, until, strict)
				itr2 = Iterators.slice(itr2, from, until, strict)
				itr3 = Iterators.slice(itr3, from, until, strict)
				this
			} else
				super.slice(from, until)
	}

	//Validation of even length hurts performance
	private final class ZipEven[+A, +B](private[this] var i1 :Iterator[A], private[this] var i2 :Iterator[B])
		extends AbstractIterator[(A, B)] with SugaredIterator[(A, B)]
	{
		{
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			if (size1 >= 0 & size2 >= 0 & size1 != size2)
				noSuch_!(
					i1.toString + " is of size " + size1 + ", while " + i2 + " is of size " + size2
				)
		}
		override def hasFastDrop = HasFastSlice(i1) && HasFastSlice(i2)
		override def knownSize :Int = {
			//We must know both sizes to guarantee an exception will be thrown if they are unequal, because
			// a common usage pattern is to iterate by counting elements until knownSize, rather than using hasNext.
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			if (size1 == size2) size1
			else -1
		}
		override def hasNext :Boolean = {
			val has1 = i1.hasNext
			val has2 = i2.hasNext
			if (has1 != has2)
				noSuch_!("Sizes of iterators differ")
			has1
		}
		override def next() :(A, B) = (i1.next(), i2.next())

		override def strictDrop(n :Int) :Iterator[(A, B)] =
			if (HasFastSlice.hasFastDrop(i1) && HasFastSlice.hasFastDrop(i2)) {
				i1 = i1.strictDrop(n)
				i2 = i2.strictDrop(n)
				this
			} else
				super.strictDrop(n)

		override def drop(n :Int) :Iterator[(A, B)] =
			if (n > 0 & HasFastSlice.hasFastDrop(i1) && HasFastSlice.hasFastDrop(i2)) {
				i1 = i1.drop(n)
				i2 = i2.drop(n)
				this
			} else
				super.drop(n)

		override def toString :String= "(" + i1 + " zipEven " + i2 + ")"
	}

	private final class ZipEven3[A, B, C](i1 :Iterator[A], i2 :Iterator[B], i3 :Iterator[C])
		extends AbstractZip3[A, B, C](i1, i2, i3)
	{
		{
			val size1 = itr1.knownSize
			val size2 = itr2.knownSize
			val size3 = itr3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0 & (size1 != size2 | size2 != size3))
				noSuch_!(
					"Iterators " + itr1 + ", " + itr2 + ", " + itr3 + " have different sizes: " +
						size1 + ", " + size2 + ", " + size3 + "."
				)
		}
		override def knownSize :Int = {
			val size1 = itr1.knownSize
			val size2 = itr2.knownSize
			val size3 = itr3.knownSize
			if (size1 == size2 & size2 == size3) size1
			else -1
		}
		override def hasNext :Boolean = {
			val has1 = hasNext1
			val has2 = hasNext2
			val has3 = hasNext3
			if (has1 != has2 | has2 != has3)
				noSuch_!("Sizes of iterators differ")
			has1
		}
		override def next() :(A, B, C) = (itr1.next(), itr2.next(), itr3.next())

		override def toString :String= i1.toString + ".zipEven(" + i2 + ", " + i3 + ")"
	}


	private final class Zip3[A, B, C](i1 :Iterator[A], i2 :Iterator[B], i3 :Iterator[C])
		extends AbstractZip3[A, B, C](i1, i2, i3)
	{
		override def knownSize :Int = {
			val size1 = itr1.knownSize
			val size2 = itr2.knownSize
			val size3 = itr3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0)
				math.min(size1, math.min(size2, size3))
			else
				-1
		}
		override def hasNext :Boolean = hasNext1 && hasNext2 && hasNext3
		override def next() :(A, B, C) = (itr1.next(), itr2.next(), itr3.next())

		override def toString :String = "Iterator.zip3(" + i1 + ", " + i2 + ", " + i3 + ")"
	}

	private final class ZipAll3[A, B, C](i1 :Iterator[A], pad1 :A, i2 :Iterator[B], pad2 :B, i3 :Iterator[C], pad3 :C)
		extends AbstractZip3[A, B, C](i1, i2, i3)
	{
		override def knownSize :Int = {
			val size1 = itr1.knownSize
			val size2 = itr2.knownSize
			val size3 = itr3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0)
				math.max(size1, math.max(size2, size3))
			else
				-1
		}
		override def hasNext :Boolean = hasNext1 || hasNext2 || hasNext3

		override def next() :(A, B, C) = {
			val a = if (itr1.hasNext) itr1.next() else pad1
			val b = if (itr2.hasNext) itr2.next() else pad2
			val c = if (itr3.hasNext) itr3.next() else pad3
			(a, b, c)
		}
		override def toString :String =
			"Iterator.zipAll3(" + i1 + "++" + pad1 + "*, " + i2 + "++" + pad2 + "*, "  + i3 + "++" + pad3 + "*)"
	}

	private final class ZipTail[+E](private[this] var underlying :Iterator[E])
		extends AbstractIterator[(E, E)] with IteratorWithDrop[(E, E)]
	{
		private[this] var prev = underlying.next()

		override def hasFastDrop :Boolean = HasFastSlice.hasFastDrop(underlying)
		override def knownSize   :Int = underlying.knownSize
		override def hasNext     :Boolean = underlying.hasNext

		override def next() :(E, E) = {
			val e = underlying.next()
			val res = (prev, e)
			prev = e
			res
		}
		override def take(n :Int) :Iterator[(E, E)] = {
			underlying = underlying.take(n)
			this
		}
		override def strictDrop(n :Int) :Iterator[(E, E)] = {
			if (n > 0) {
				if (n > 1)
					underlying = underlying.strictDrop(n - 1)
				if (underlying.hasNext)
					prev = underlying.next()
			}
			this
		}
		override def toString :String = underlying.toString + ".zipTail"
	}


	/** Removes duplicates in favour of their first occurrence. */
	private final class FirstOccurrences[+E](private[this] var underlying :Iterator[E])
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		private[this] val seen = new mutable.HashSet[E]

		@tailrec private def advance() :Boolean =
			underlying.hasNext && {
				val elem = underlying.next()
				seen.add(elem) && push(elem) || advance()
			}
		override def hasNext :Boolean = super.hasNext || advance()

		override def toString :String = underlying.toString + ".unique"
	}

	/** Combined implementation of `filter` and `filterNot`, keeping elements for which `test` equals `expect`. */
	private final class Keep[E](private[this] var underlying :Iterator[E], test :Int => Boolean, expect :Boolean)
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		private[this] var i = -1

		@tailrec private def advance() :Boolean =
			underlying.hasNext && ({ i += 1; test(i) == expect } && push(underlying.next()) || {
				underlying.next()
				advance()
			})
		override def hasNext :Boolean = super.hasNext || advance()

		override def toString :String = underlying.toString + ".keep(" + test + ")"
	}


	private final class Removed[+E](private[this] var underlying :Iterator[E], index :Int,
	                                private[this] var validating :Boolean = true)
		extends AbstractBufferedIterator[E] with SingleDrop[E]
	{   //todo: don't extend AbstractBufferedIterator, it only complicates the implementation.
		//The number of consumed items from underlying, not the number of consumed items from this.
		// Counting stops after passing index in order not to overflow.
		private[this] var i = 0
		def offset :Int = i
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying)

		override def knownSize :Int = {
			val size = underlying.knownSize
			if (size < 0) -1
			else if (i > index) size + super.hasNext.toInt
			else if (i > index - size) size - 1 + super.hasNext.toInt
			else if (validating) outOfBounds_!(index, i + size)
			else size + super.hasNext.toInt
		}
		override def hasNext :Boolean = super.hasNext || {
			if (i == index)
				if (underlying.hasNext) {
					underlying.next()
					i += 1
				} else
					validating && ioob()
			underlying.hasNext && {
				i += 1; push(underlying.next())
			} || {
				if (i < index & validating)
					outOfBounds_!(index, i)
				false
			}
		}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else  {
				var toDrop = n
				if (super.hasNext) {             //Essentially, we need to reduce n by 1.
					toDrop -= 1
					pop()
				}
				if (i > index)                   //Unwrap ourselves, as we are past the removed index.
					Iterators.drop(underlying, toDrop, strict)
				else if (toDrop < index - i) {
					underlying = Iterators.drop(underlying, toDrop, strict)
					i += n
					this
				} else if (strict) {             //We need to drop past the removed index.
					val dropAfterIndex = toDrop - index + i
					underlying = Iterators.drop(underlying, toDrop, strict)
					i = index
					if (hasNext)                 //hasNext will throw an exception if index is out of range.
						drop(dropAfterIndex - 1) //hasNext already has dropped the first (that is, index-th) element.
					else
						this
				} else
					super[AbstractBufferedIterator].drop(toDrop)
			}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else if (super.hasNext) {
				underlying = underlying.take(n - 1)
				if (n <= index - i + 1)
					validating = false
				this
			} else if (n <= index - i)
				underlying.take(n)
			else {
				underlying = underlying.take(n)
				this
			}
		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 || start >= xs.length)
				0
			else {
				var offset = start
				var count  = len
				if (super.hasNext) {
					xs(start) = next()
					offset += 1
					count  -= 1
				}
				if (i > index)
					underlying.copyToArray(xs, offset, count)
				else {
					val max = math.min(count, xs.length - offset)
					if (max <= index - i || { val k = underlying.knownSize; k >= 0 & k <= index - i })
						underlying.copyToArray(xs, offset, max)
					else { //This could be better with slicer iterator
						val (before, after) = underlying.splitAt(index - i)
						val copiedBefore = before.copyToArray(xs, offset, index - i)
						if (validating && !after.hasNext) {
							i += copiedBefore
							ioob()
						}
						if (copiedBefore < index - i || !after.hasNext)
							copiedBefore
						else {
							assert(copiedBefore == index - i,
								iterator.toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + (index - i) +
									") copied more than the maximum number of elements: " + copiedBefore
							)
							after.next()
							i = index + 1
							copiedBefore + after.copyToArray(xs, offset + copiedBefore, max - copiedBefore)
						}
					}
				}
			}
		private def ioob() = outOfBounds_!(toString + ": " + index + " out of " + i)

		override def toString :String =
			if (super.hasNext)
				"Iterator@" + i + "(" + head + ",...)"
			else
				underlying.toString + "@" + i + ".removed(" + index + ")"
	}

	//An overflow conscious implementation.
	private final class RemovedSlice[+E](private[this] var underlying :Iterator[E], from :Int, until :Int)
		extends AbstractBufferedIterator[E] with SingleDrop[E]
	{
		private[this] var i = 0
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying)

		override def knownSize :Int = {
			val size = underlying.knownSize
			if (size < 0)
				-1
			else if (i >= until)
				size + super.hasNext.toInt
			else {
				val start = math.min(from, i + size)
				val end   = math.min(math.max(until, start), i + size)
				size + super.hasNext.toInt - (end - start)
			}
		}
		override def hasNext :Boolean = super.hasNext || {
			if (i == from) {
				underlying = underlying.drop(until - from)
				i = until
			}
			underlying.hasNext && {
				i += 1
				push(underlying.next())
			}
		}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else {
				var toDrop = n
				if (super.hasNext) {
					toDrop -= 1
					pop()
				}
				if (i < until & toDrop > from - i) {
					toDrop += math.min(until - from, Int.MaxValue - toDrop)
					Iterators.drop(underlying, toDrop, strict)
				} else {
					underlying = Iterators.drop(underlying, toDrop, strict)
					i += math.min(toDrop, Int.MaxValue - i)
					this
				}
			}

		override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int =
			if (len <= 0 | from == 0 & until == Int.MaxValue || start >= xs.length) //overflow guard
				0
			else if (super.hasNext) {
				xs(start) = head
				pop()
				1 + copyToArray(xs, start + 1, len - 1)
			} else if (i >= until)
				underlying.copyToArray(xs, start, len)
			else {
				val size = knownSize
				val max =
					if (size < 0) math.min(len, xs.length - start)
					else math.min(len, math.min(size, xs.length - start))
				if (max <= from - i)
					underlying.copyToArray(xs, start, max)
				else {
					val (before, after) = underlying.splitAt(from - i)
					val copiedBefore    = before.copyToArray(xs, start, from - i)
					underlying          = after.drop(until - from)
					copiedBefore + underlying.copyToArray(xs, start + copiedBefore, max - copiedBefore)
				}
			}

		override def toString :String =
			if (super.hasNext)
				"Iterator@" + i + "(" + head + ",...)"
			else
				underlying.toString + "@" + i + ".removed(" + from + ".." + until + ")"
	}

	private final class Updated[+E](private[this] var underlying :Iterator[E], index :Int, elem :E)
		extends AbstractIterator[E] with SingleDrop[E]
	{
		private[this] var i = 0
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying)
		override def knownSize :Int = underlying.knownSize

		override def hasNext :Boolean =
			underlying.hasNext || i <= index && ioob()

		override def next() :E = {
			if (index >= i && !underlying.hasNext)
				outOfBounds_!(index, i)
			else if (i == index) {
				i += 1
				underlying.next()
				elem
			} else {
				val res = underlying.next()
				i += 1
				res
			}
		}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else if (i > index)                                    //Unwrap ourselves
				underlying.drop(n)
			else if (n <= index - i) {
				underlying = Iterators.drop(underlying, n, strict)
				i += n
				this
			} else if (strict) {                                   //Dropping past the updated index
				strictDrop(index - i)
				hasNext                                            //Validates and throws an exception if empty.
				Iterators.drop(underlying, n - index - i, strict)  //Now that we are past index, we can unwrap ourselves
			} else
				super[AbstractIterator].drop(n)

		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else if (i > index | n < index - i) //The iterator is cut off before the updated index, so there is no need
				underlying.take(n)              // for us, and we don't want to trigger an IndexOutOfBoundsException.
			else {
				underlying = underlying.take(n)
				this
			}
		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i > index)
				underlying.copyToArray(xs, start, len)
			else if (len <= 0 || start >= xs.length)
				0
			else if (i == index)
				if (!underlying.hasNext)
					ioob()
				else {
					xs(start) = elem
					underlying.next()
					1 + underlying.copyToArray(xs, start + 1, len - 1)
				}
			else {
				val max = math.min(len, xs.length - start)
				if (max <= index - i)
					underlying.copyToArray(xs, start, len)
				else {
					val (before, after) = underlying.splitAt(index - i)
					val copiedBefore = before.copyToArray(xs, start, index - i)
					if (!before.hasNext)
						outOfBounds_!(
							toString + ": " + index + " out of between " + i + " and " + index
						)
					assert(copiedBefore == index - i,
						iterator.toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + (index - i) +
							") copied more than the maximum number of elements: " + copiedBefore
					)
					xs(start + copiedBefore) = elem
					after.next()
					copiedBefore + 1 + after.copyToArray(xs, start + copiedBefore + 1, max - copiedBefore - 1)
				}
			}
		private def ioob() = outOfBounds_!(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + ".updated(@" + (index - i) + "=" + elem + ")"
	}

	private final class UpdatedAll[+E](private[this] var underlying :Iterator[E], index :Int,
	                                   private[this] var elems :Iterator[E])
		extends AbstractIterator[E] with SingleDrop[E]
	{
		if (index < 0)
			outOfBounds_!(toString)

		private[this] var i = 0
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying) && HasFastSlice.hasFastDrop(elems)

		override def knownSize :Int = {
			val size      = underlying.knownSize
			val elemsSize = elems.knownSize
			//If knownSize == 0 then IterableFactory.from will usually return an empty collection without iterating
			if (size >= 0 && index + size < i + math.max(elemsSize, 0))
				outOfBounds_!("Iterator.empty.updatedAll(@" + index + "=" + elems + ")")
			else if (elemsSize < 0)
				-1
			else
				size
		}
		override def hasNext :Boolean =
			underlying.hasNext || {
				if (index > i || elems.hasNext) //updateAll(length, Nil) is allowed
					outOfBounds_!(toString)
				false
			}
		override def next() :E =
			if (i >= index)
				if (elems.hasNext) {
					if (!underlying.hasNext)
						ioob()
					i += 1
					underlying.next()
					elems.next()
				} else //don't increase i to avoid overflow
					underlying.next()
			else if (!underlying.hasNext)
				outOfBounds_!(index, i)
			else {
				i += 1
				underlying.next()
			}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else if (i >= index && !elems.hasNext)
				underlying.take(n)
			else if (n <= index - i) //We cut off before index, so revert to the underlying iterator instead of this one
				underlying.take(n)
			else {                   //We need to cut off the updated elements accordingly, so as not to fail validation.
				underlying = underlying.take(n)
				elems = elems.take(n + i - index)
				this
			}

		//Obligation of validation makes this less efficient than it could be.
		// We should probably drop the requirement, or at least have a 'feature' that it is silently switched off by drop.
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else if (i >= index && !elems.hasNext)
				underlying.drop(n)
			else if (i >= index) {
				val remaining = elems.knownSize
				if (remaining > n) {                //We can drop from both iterators without disabling validation.
					elems = elems.drop(n)
					underlying = Iterators.drop(underlying, n, strict)
					this
				} else if (remaining < 0)
					super[AbstractIterator].drop(n) //Slow drop so we don't switch off validation.
				else if (remaining == 0)
					underlying.drop(n)
				else {                              //Drop up to the last updated element to see if index is in range.
					elems = Iterators.drop(elems, remaining - 1, strict)
					underlying = Iterators.drop(underlying, remaining - 1, strict)
					if (!underlying.hasNext)
						outOfBounds_!(
							toString + ": " + (index + remaining - 1) + " out of between " + i + " and " + (i + remaining - 1)
						)
					Iterators.drop(underlying, n - remaining + 1, strict)
				}
			} else if (n <= index - i) {
				underlying = underlying.drop(n)
				i += n
				this
			} else {                               //Reduce to the previous case.
				underlying = Iterators.drop(underlying, index - 1, strict)
				i = index
				drop(n - index + i, strict)
			}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i >= index && !elems.hasNext)
				underlying.copyToArray(xs, start, len)
			else if (len <= 0 || start >= xs.length)
				0
			else {
				val max = math.min(len, xs.length - start)
				if (i >= index) {
					val k = elems.knownSize
					val updatedCopied =
						if (k >= 0 && k < max) {
							val updatedCopied = elems.copyToArray(xs, start, max)
							elems = Iterator.empty
							assert(updatedCopied == k,
								elems.toString + " of knownSize = " + k + " copied " + updatedCopied + " elements, to " +
									errorString(xs) + " starting at " + start + ", with the maximum of " + len + "."
							)
							updatedCopied
						} else {
							val (copied, left) = elems.splitAt(max)
							elems = left
							copied.copyToArray(xs, start, max)
						}
					i += updatedCopied
					underlying = underlying.drop(updatedCopied - 1)
					if (!underlying.hasNext)
						ioob()
					underlying.next()
					updatedCopied + underlying.copyToArray(xs, start + updatedCopied, max - updatedCopied)
				} else if (max <= index - i)
					underlying.copyToArray(xs, start, len)
				else {
					val (before, after) = underlying.splitAt(index - i)
					underlying = after
					val copiedBefore = before.copyToArray(xs, start, index - i)
					if (copiedBefore < index - i) {
						i = index
						ioob()
					}
					assert(copiedBefore == index - i,
						iterator.toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + (index - i) +
							") copied more than the maximum number of elements: " + copiedBefore
					)
					i = index
					copiedBefore + copyToArray(xs, start + copiedBefore, max - copiedBefore)
				}
			}
		private def ioob() = outOfBounds_!(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + ".updatedAll(@" + (index - i) + "=" + elems + ")"
	}


	private final class Overwritten[+E](private[this] var underlying :Iterator[E], index :Int,
	                                    private[this] var elems :Iterator[E])
		extends AbstractIterator[E] with SingleDrop[E]
	{
		private[this] var i = 0
		if (index < 0)
			elems = elems.drop(-index)

		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying) && HasFastSlice.hasFastDrop(elems)

		override def knownSize = underlying.knownSize

		override def hasNext :Boolean = underlying.hasNext
		override def next() :E =
			if (!underlying.hasNext)
				noSuch_!(toString + ".next()")
			else if (i >= index)
				if (elems.hasNext) {
					underlying.next()
					elems.next()
				} else
					underlying.next()
			else {
				i += 1
				underlying.next()
			}

		override def take(n :Int) :Iterator[E] = {
			if (n > 0)
				underlying = underlying.take(n)
			this
		}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] = {
			if (n > 0) {
				if (i > index)
					elems = Iterators.drop(elems, n, strict)
				underlying = Iterators.drop(underlying, n, strict)
			}
			this
		}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i >= index)
				if (elems.hasNext) {
					val rem = elems.knownSize
					if (rem >= 0) {
						underlying = underlying.drop(rem)
						val copied = elems.copyToArray(xs, start, len)
						copied + underlying.copyToArray(xs, start + copied, len - copied)
					} else {
						val max = math.min(len, xs.length - start)
						var copied = 0
						while (copied < max && underlying.hasNext && elems.hasNext) {
							xs(copied) = elems.next()
							underlying.next()
							copied += 1
						}
						underlying.copyToArray(xs, start + copied, max - copied)
					}
				} else
					underlying.copyToArray(xs, start, len)
			else if (len <= 0 || start >= xs.length)
				0
			else {
				val size = underlying.knownSize
				val max =
					if (size >= 0) math.min(math.min(len, size), xs.length - start)
					else math.min(len, xs.length - start)
				val copied =
					if (max <= index - i)
						underlying.copyToArray(xs, start, len)
					else {
						val (before, after) = underlying.splitAt(index - i)
						underlying = after
						before.copyToArray(xs, start, len)
					}
				i += copied
				copied + copyToArray(xs, start + copied, max - copied)
			}
		override def toString :String = underlying.toString + ".overwritten(@" + (index - i) + "=" + elems + ")"
	}


	private final class Inserted[+E](private[this] var underlying :Iterator[E], index :Int, elem :E)
		extends AbstractIterator[E] with SingleDrop[E]
	{
		private[this] var i = 0
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying)

		override def knownSize :Int = {
			val k = underlying.knownSize
			if (k < 0) -1
			else if (k < index - i) outOfBounds_!(index.toString + " out of " + (i + k))
			else k + 1
		}
		override def hasNext :Boolean = underlying.hasNext || index == i || index > i && ioob()
		override def next() :E = {
			val res =
				if (i == index)
					elem
				else if (underlying.hasNext)
					underlying.next()
				else
					ioob()
			i += 1
			res
		}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else if (i > index)
				underlying.drop(n)
			else if (n < index - i) {
				i += n
				underlying = Iterators.drop(underlying, n, strict)
				this
			} else {
				underlying = Iterators.drop(underlying, index - i - 1, strict)
				if (!underlying.hasNext)
					outOfBounds_!(index.toString + " out of between " + i + " and " + (index - 1))
				underlying.next()
				if (n == index - i) {
					i = index
					this
				} else
					Iterators.drop(underlying, n - index + i - 1, strict)
			}

		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else if (i > index) //otherwise we'd throw an IndexOutOBoundsException
				underlying.take(n)
			else if (n <= index - i) {
				underlying = underlying.take(n)
				this
			} else {
				underlying = underlying.take(n - 1)
				this
			}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i > index)
				underlying.copyToArray(xs, start, len)
			else if (len <= 0 || start >= xs.length)
				0
			else {
				val max = math.min(len, xs.length - start)
				if (index == i) {
					xs(start) = elem
					i += 1
					1 + copyToArray(xs, start + 1, max - 1)
				} else if (max <= index - i)
					underlying.copyToArray(xs, start, len)
				else if (max == index - i + 1) {
					val copiedBefore = underlying.copyToArray(xs, start, len)
					i += copiedBefore
					if (copiedBefore <= max)
						ioob()
					xs(start + copiedBefore) = elem
					1 + copiedBefore
				} else {
					val (before, after) = underlying.splitAt(index - i)
					val copiedBefore = before.copyToArray(xs, start, len)
					if (copiedBefore < index - i) {
						i += copiedBefore
						ioob()
					}
					xs(start + copiedBefore) = elem
					i += copiedBefore + 1
					copiedBefore + 1 + after.copyToArray(xs, start + copiedBefore + 1, max - copiedBefore - 1)
				}
			}
		private def ioob() = outOfBounds_!(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + "@" + i + ".inserted(" + index + ", " + elem + ")"
	}

	//todo: make indexing permissive
	private final class InsertedAll[+E](private[this] var underlying :Iterator[E], index :Int, private[this] var elems :Iterator[E])
		extends AbstractIterator[E] with SingleDrop[E]
	{
		private[this] var i = 0
		override def hasFastDrop = HasFastSlice.hasFastDrop(underlying) && HasFastSlice.hasFastDrop(elems)

		override def knownSize :Int = {
			val remaining = underlying.knownSize
			if (remaining >= 0 & remaining < index - i)
				outOfBounds_!(toString + ": " + index + " out of " + (i + remaining))
			var inserted = elems.knownSize
			if (inserted < 0 && !elems.hasNext)
				inserted = 0
			if (remaining >= 0 && inserted >= 0)
				remaining + inserted
			else
				-1
		}
		override def hasNext :Boolean = underlying.hasNext || index == i && elems.hasNext || index > i && ioob()

		override def next() :E =
			if (i == index)
				if (elems.hasNext)
					elems.next()
				else {
					i += 1
					underlying.next()
				}
			else if (underlying.hasNext) {
				i += 1
				underlying.next()
			} else
				ioob()

		override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else if (i >= index && !elems.hasNext)
				Iterators.drop(underlying, n, strict)
			else if (i >= index) {
				val remaining = elems.knownSize
				if (remaining >= n) {
					elems = Iterators.drop(elems, n, strict)
					this
				} else if (remaining < 0) {
					if (strict) {
						var dropped = 0
						while (dropped < n && elems.hasNext) {
							elems.next()
							dropped += 1
						}
						if (!elems.hasNext)
							underlying.drop(n - dropped)
						else
							this
					} else
						super[AbstractIterator].drop(n)
				} else {
					elems = Iterator.empty
					underlying.drop(n - remaining)
				}
			} else if (n < index - i) {
				underlying = Iterators.drop(underlying, n, strict)
				i += n
				this
			} else {
				underlying = Iterators.drop(underlying, index - i - 1, strict)
				if (!underlying.hasNext)
					outOfBounds_!(
						toString + ": " + index + " out of between " + i + " and " + (index - 1)
					)
				underlying.next()
				val leftToDrop = n - index + 1
				i = index
				drop(leftToDrop, strict)
			}

		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else if (n <= index - i)
				underlying.take(n)
			else {
				val k = elems.knownSize
				if (k < 0)
					if (i >= index && !elems.hasNext)
						underlying.take(n)
					else
						super.take(n)
				else if (i >= index)
					if (k >= n)
						elems.take(n)
					else {
						underlying = underlying.take(n - k)
						this
					}
				else {
					if (k > index - i + n) {
						elems = elems.take(i - index + n)
					} else
						underlying = underlying.take(n - k)
					this
				}
			}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i >= index && !elems.hasNext)
				underlying.copyToArray(xs, start, len)
			else if (len <= 0 || start > xs.length)
				0
			else {
				val max = math.min(len, xs.length - start)
				if (i >= index) {
					val copied = elems.copyToArray(xs, start, max)
					if (copied == max)
						copied
					else
						copied + underlying.copyToArray(xs, start + copied, max - copied)
				} else if (max <= index - i)
					underlying.copyToArray(xs, start, len)
				else {
					val k = elems.knownSize
					if (k >= max - index + i) {
						val before = underlying.copyToArray(xs, start, index - i)
						if (before < index - i)
							outOfBounds_!(
								toString + ": " + index + " out of between " + (i + before)
							)
						elems.copyToArray(xs, start + before, max + before)
					} else {
						val (before, after) = underlying.splitAt(index - i)
						val copied = before.copyToArray(xs, start, len)
						if (copied < index - i)
							outOfBounds_!(
								toString + ": " + index + " out of between " + (i + copied)
							)
						underlying = after
						i = index
						copied + copyToArray(xs, start + copied, max - copied)
					}
				}
			}

		private def ioob() = outOfBounds_!(toString + ": " + index + " out of " + i)

		override def toString :String = {
			val elemsSize = elems.knownSize
			val elemsString = if (elemsSize >= 0) elems.className +  "|" + elemsSize + "|" else elems.className
			underlying.toString + "@" + i + ".insertedAll(" + index + ", " +  elemsString + ")"
		}
	}


	private final class IteratorPair[+E](private[this] var current :Iterator[E], private[this] var nxt :Iterator[E])
		extends AbstractSugaredIterator[E]
	{
		override def knownSize :Int = {
			val s1 = current.knownSize
			val s2 = nxt.knownSize
			if (s1 < 0 | s2 < 0) -1 else s1 + s2
		}
		override def hasNext = current.hasNext || nxt.hasNext
		override def next() :E = {
			if (!current.hasNext) {
				current = nxt
				nxt = Iterator.empty
			}
			current.next()
		}

		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else {
				val first = current.knownSize
				if (first <= 0)
					super.take(n)
				else if (first < 0) {
					current = current.take(n)
					this
				} else {
					nxt = nxt.take(n - first)
					this
				}
		}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				val first = current.knownSize
				if (first < 0)
					super.drop(n)
				else if (first >= n) {
					current = current.drop(n)
					this
				} else {
					current = nxt.drop(n - first)
					nxt = Iterator.empty
					this
				}
			}

		def append[U >: E](itr :Iterator[U]) :Iterator[U] =
			if (itr.knownSize == 0)
				this
			else if (current.knownSize == 0)
				if (nxt.knownSize == 0) itr else new IteratorPair(nxt, itr)
			else if (nxt.knownSize == 0)
				new IteratorPair(current, itr)
			else
				new Concat(current, nxt, itr)

		def prepend[U >: E](itr :Iterator[U]) :Iterator[U] =
			if (itr.knownSize == 0)
				this
			else if (current.knownSize == 0)
				if (nxt.knownSize == 0) itr else new IteratorPair(itr, nxt)
			else if (nxt.knownSize == 0)
				new IteratorPair(itr, current)
			else
				new Concat(itr, current, nxt)

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 || start >= xs.length)
				0
			else {
				val copied = current.copyToArray(xs, start, len)
				copied + nxt.copyToArray(xs, start + copied, len - copied)
			}

		override def toString :String = {
			val s1 = current.knownSize
			val s2 = current.knownSize
			if (s1 == 0 && s2 == 0)
				"Iterator()"
			else if (s1 == 0)
				nxt.toString
			else if (s2 == 0)
				current.toString
			else
				current.toString + ":++" + nxt.toString
		}

	}

	//consider: integrating the idea behind RelayArray and including the array directly.
	// We could also use LightStack for best performance, but the original iterators would need to throw an exception.
	//We could avoid calling tail on iterators by having an index, but that would prevent past iterators
	// to be garbage collected. However, any 'parent' Concats use the same iterators,
	// so we could replace individual iterators with Iterator.empty.
	// Alternatively, we could simply warn that the original iterator's state becomes undefined when
	private final class Concat[+E](private[this] var iterators :IndexedSeq[Iterator[E]])
		extends AbstractFlatMap[E] with SingleDrop[E]
	{
//		def this(iterators :PassedArray[Iterator[E]]) = this(iterators.head, iterators.tail)
		def this(first :Iterator[E], second :Iterator[E]) = this(RelayArray.two(first, second))
		def this(first :Iterator[E], second :Iterator[E], third :Iterator[E]) = this({
			val a = new Array[Any](3)
			a(0) = first
			a(1) = second
			a(2) = third
			RelayArray.wrap(a.asInstanceOf[IRefArray[Iterator[E]]])
		})

		private def list :IndexedSeq[Iterator[E]] = iterators
		if (iterators.nonEmpty)
			enqueue(iterators.head)

		@tailrec protected override def enqueue(items :Iterator[E @uncheckedVariance]) :Boolean =
			items match {
				case concat :Concat[E] =>
					if (concat.list.isEmpty)
						super.enqueue(Iterator.empty)
					else {
						iterators = concat.list.tail ++: iterators
						enqueue(concat.list.head)
					}
				case _ => super.enqueue(items)
			}
		override def knownSize :Int = {
			var i = 0; val end = iterators.length
			var size = 0
			while (i < end && { val s = iterators(i).knownSize; size += s; s >= 0 })
				i += 1
			if (i == end) size
			else -1
		}

		def append[U >: E](next :Iterator[U]) :Concat[U] =
			if (!next.hasNext) this else new Concat(iterators :+ next)

		def prepend[U >: E](prev :Iterator[U]) :Concat[U] =
			if (!prev.hasNext) this else new Concat(prev +: iterators)

		override def hasNext :Boolean = super.hasNext || {
			var i = 0; val iterCount = iterators.length
			while (i < iterCount && !enqueue(iterators(i)))
				i += 1
			iterators = iterators.drop(i)
			i < iterCount
		}

		override def drop(n :Int, strict :Boolean) :Iterator[E] =
			if (n <= 0)
				this
			else {
				val count = iterators.length
				var rem   = n
				var i     = 0
				while (rem > 0 && i < count && {
					val size = iterators(i).knownSize
					size >= 0 && { rem -= size; true }
				}) {
					i += 1
				}
				//All iterators in iterators.take(i) had known size and are dropped at this point.
				if (i == count)
					if (rem >= 0)
						Iterator.empty
					else {
						val last = iterators(count - 1)
						Iterators.drop(last, last.knownSize + rem, strict)
					}
				else if (rem == 0) {
					iterators = iterators.drop(i)
					enqueue(iterators(0))
					this
				} else if (rem < 0) {
					iterators = iterators.drop(i - 1)
					val head  = iterators(0)
					val dropped = Iterators.drop(head, head.knownSize + rem, strict)
					if (dropped ne head)
						iterators = iterators.updated(0, dropped)
					enqueue(dropped)
					this
				} else {
					iterators = iterators.drop(i)
					enqueue(iterators(0))
					super.drop(rem)
				}
			}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				val count = iterators.length
				var rem   = n
				var i     = 0
				while (i < count && rem > 0) {
					val k = iterators(i).knownSize
					if (k >= 0)
						rem -= k
					else
						i = count
					i += 1
				}
				if (i == count + 1)
					super.take(n)
				else if (rem == 0) {
					iterators = iterators.take(i)
					this
				} else {
					val last  = iterators(i - 1)
					iterators = iterators.take(i - 1) :+ last.take(last.knownSize + rem)
					this
				}
			}
		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
			val max = math.min(len, xs.length - start)
			var copied = 0
			val iterCount = iterators.length
			var i = 0
			while (copied < max & i < iterCount) {
				copied += iterators(i).copyToArray(xs, start + copied, max - copied)
				i += 1
			}
			copied
		}
	}

	private class ReverseSortedSetIterator[+E](set :collection.SortedSet[E])
		extends IteratorTake[E](set.size)
	{
		private[this] var last :Option[E] = set.lastOption
		override def next() :E = {
			val res = last.get
			last = set.maxBefore(res)
			knownSize_--()
			res
		}
		override def toString :String = last match {
			case Some(elem) => "Iterator|" + knownSize + "|(->" + elem + ",...)"
			case _          => "Iterator()"
		}
	}

	private class ReverseSortedMapIterator[K, V](map :collection.SortedMap[K, V])
		extends IteratorTake[(K, V)](map.size)
	{
		private[this] var last :Option[(K, V)] = map.lastOption
		override def next() :(K, V) = {
			val res = last.get
			last = map.maxBefore(res._1)
			knownSize_--()
			res
		}
		override def toString :String = last match {
			case Some(elem) => "Iterator|" + knownSize + "|(->" + elem + ",...)"
			case _          => "Iterator()"
		}
	}



	private class TakeRight[+E](private[this] var underlying :Iterator[E], n :Int,
	                            private[this] var queue :mutable.Queue[E] = null)
		extends AbstractIterator[E] with IteratorWithDrop[E]
	{
		//Equals queue.size if queue != null, otherwise equals the number of elements in underlying, if known.
		private[this] var count = underlying.knownSize

		override def knownSize :Int = if (count >= 0) math.min(count, n) else -1

		override def hasNext :Boolean = underlying.hasNext || queue != null && queue.nonEmpty

		override def next() :E = {
			if (count >= 0 & queue == null) {
				underlying = underlying.drop(count - n)
				count = math.min(n, count) - 1
				underlying.next()
			} else {
				if (queue == null)
					enqueue()
				count -= 1
				queue.dequeue()
			}
		}
		private def enqueue() :Unit = {
			queue = mutable.Queue.empty[E]
			count = 0
			while (count < n && underlying.hasNext) {
				queue += underlying.next()
				count  += 1
			}
			while (underlying.hasNext) {
				queue.dequeue()
				queue += underlying.next()
			}
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0) {
				if (count >= 0 & queue == null) {
					underlying = underlying.drop(count - this.n + n)
					count = math.max(this.n - n, 0)
				} else {
					if (queue == null)
						enqueue()
					count = math.max(count - n, 0)
					queue.dropInPlace(n)
				}
			}
			this
		}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len < 0 || count == 0 || start >= xs.length)
				0
			else if (count > 0 & queue == null) {
				underlying = underlying.drop(count - n)
				underlying.copyToArray(xs, start, len)
			} else {
				if (queue == null)
					enqueue()
				queue.copyToArray(xs, start, len)
			}

		override def toString :String = underlying.toString + ".takeRight(" + n + ")"
	}

	private class TakeUntil[A, +E](private[this] var underlying :Iterator[E],
	                               private[this] var acc :A, pred :A => Boolean, op :(A, E) => A)
		extends AbstractIterator[E] with SlowDropIterator[E]
	{
		private[this] var done = false
		override def hasNext :Boolean = !done && underlying.hasNext
		override def next() :E =
			if (done)
				noSuch_!("Iterator.empty.next")
			else {
				val next = underlying.next()
				acc = op(acc, next)
				done = pred(acc)
				next
			}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else {
				underlying = underlying.take(n)
				this
			}
		override def toString :String = underlying.toString + ".takeUntil(" + acc + ")(" + pred + ")(" + op + ")"
	}

	private class TakeWith[A, +E](private[this] var underlying :Iterator[E],
	                              private[this] var acc :A, pred :A => Boolean, op :(A, E) => A)
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		private[this] var continue = true
		override def hasNext :Boolean = super.hasNext || continue && {
			continue = underlying.hasNext && {
				val next = underlying.next()
				acc = op(acc, next)
				pred(acc) && push(next)
			}
			continue
		}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else {
				underlying = underlying.take(n)
				this
			}
		override def toString :String =
			if (super.hasNext) "Iterator(" + head + ",...)"
			else underlying.toString + ".takeWith(" + acc + ")(" + pred + ")(" + op + ")"
	}

	private class DropUntil[A, +E](private[this] var underlying :Iterator[E],
	                               private[this] var acc :A, pred :A => Boolean, op :(A, E) => A)
		extends AbstractIterator[E] with SlowDropIterator[E]
	{
		private[this] var dropped = false
		override def hasNext :Boolean =
			if (dropped) underlying.hasNext
			else { dropped = true; ff() }

		override def next() :E = {
			if (!dropped) {
				ff()
				dropped = true
			}
			underlying.next()
		}
		@tailrec private def ff() :Boolean =
			underlying.hasNext && {
				acc = op(acc, underlying.next())
				pred(acc) && underlying.hasNext || ff()
			}
		override def toString :String =
			if (dropped) underlying.toString
			else underlying.toString + ".dropUntil(" + acc + ")(" + pred + ")(" + op + ")"
	}

	private class DropWith[A, +E](private[this] var underlying :Iterator[E],
	                              private[this] var acc :A, pred :A => Boolean, op :(A, E) => A)
		extends AbstractBufferedIterator[E] with SlowDropIterator[E]
	{
		private[this] var dropped = false
		override def hasNext :Boolean =
			super.hasNext || {
				if (dropped) underlying.hasNext && push(underlying.next())
				else { dropped = true; ff() }
			}
		@tailrec private def ff() :Boolean =
			underlying.hasNext && {
				val next = underlying.next()
				acc = op(acc, next)
				if (pred(acc)) ff() else push(next)
			}
		override def toString :String =
			if (super.hasNext) underlying.toString + "(" + head + ",...)"
			else if (dropped) underlying.toString
			else underlying.toString + ".dropWhileWith(" +  acc + ")(" + pred + ")(" + op + ")"
	}

	/** The first half of `underlying.splitAt(idx)`. */
	private final class Take[E](private[this] var underlying :Iterator[E], idx :Int, private[this] var limit :Int)
		extends AbstractBufferedIterator[E] with SingleDrop[E]
	{
		def this(itr :Iterator[E], idx :Int) = this(itr, idx, idx)

		private[this] var memoized :mutable.Queue[E] = _
		private[this] var i = 0 //The current index in underlying, as long as Drop is unused

		override def hasFastDrop = memoized != null || HasFastSlice.hasFastDrop(underlying)

		def suffix :Iterator[E] = new Drop(this)

		/** Size of the ''second'' iterator returned by split, or `-1` if unknown. */
		private[Iterators] def droppedSize :Int =
			if (i >= idx | (memoized ne null))
				-1
			else {
				val k = underlying.knownSize
				if (k < 0) -1
				else if (k <= idx - i) 0
				else k - idx + i
			}
		override def knownSize :Int =
			if (i >= limit)
				super.hasNext.toInt
			else {
				val k = underlying.knownSize
				if (k < 0) -1 else math.min(k, limit - i) + super.hasNext.toInt
			}
		override def hasNext :Boolean = super.hasNext || i < limit && {
			if (memoized ne null)
				memoized.nonEmpty && { i += 1; push(memoized.dequeue()) }
			else
				underlying.hasNext && { i += 1; push(underlying.next()) }
		}
		override def take(n :Int) :Iterator[E] =
			if (n <= 0)
				Iterator.empty
			else {
				limit = math.min(limit, i + super.hasNext.toInt + n)
				this
			}
		protected override def drop(n :Int, strict :Boolean) :Iterator[E] = {
			if (n > 0 & i < limit) {
				var toDrop = math.min(n, limit - i)
				if (super.hasNext) {
					pop()
					toDrop -= 1
				}
				i += toDrop
				if (memoized ne null)
					memoized.dropInPlace(toDrop)
				else
					underlying = Iterators.drop(underlying, toDrop, strict)
			}
			this
		}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 | xs.length == 0 | start >= xs.length)
				0
			else {
				var offset = start
				val max = math.min(len, limit - i)
				if (super.hasNext) {
					xs(start) = next()
					offset = start + 1
				}
				if (memoized ne null) {
					val copied = memoized.copyToArray(xs, offset, max)
					memoized.remove(0, copied)
					i      += copied
					offset += copied
				} else {
					//Can't delegate to underlying.copyToArray because it would make it potentially unusable,
					// and thus also Drop.
					while (i < limit) {
						xs(offset) = underlying.next()
						i      += 1
						offset += 1
					}
				}
				offset - start
			}
		private[Iterators] def finish() :Iterator[E] = {
			if (i < idx) {
				if (i < limit) {
					if (memoized eq null)
						memoized = new mutable.Queue[E]
					while (i < limit && underlying.hasNext) {
						memoized += underlying.next()
						i += 1
					}
				}
				underlying = underlying.drop(idx - i)
				i = idx
			}
			underlying
		}
		override def toString :String =
			underlying.toString + ".take(" + (math.max(limit - i, 0) + super.hasNext.toInt) + ")"
	}

	/** Second iterator from `taken.underlying.splitAt`. */
	class Drop[E](taken :Take[E]) extends AbstractIterator[E] with SugaredIterator[E] {
		private[this] var underlying :Iterator[E] = _
		override def knownSize :Int =
			if (underlying == null) taken.droppedSize else underlying.knownSize

		override def hasNext :Boolean = {
			val k = taken.droppedSize
			k > 0 || k < 0 && {
				ff()
				underlying.hasNext
			}
		}
		override def next() :E = {
			ff()
			underlying.next()
		}
		override def strictDrop(n :Int) :Iterator[E] =
			if (n <= 0 || taken.droppedSize == 0)
				this
			else {
				ff()
				HasFastSlice.drop(underlying, n)
			}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0 || taken.droppedSize == 0)
				this
			else if (underlying ne null)
				underlying.drop(n)
			else
				LazyIterator { ff(); underlying.drop(n) }

		override def take(n :Int) :Iterator[E] =
			if (n <= 0 || taken.droppedSize == 0)
				Iterator.empty
			else if (underlying ne null)
				underlying.take(n)
			else
				LazyIterator { ff(); underlying.take(n) }

		override def strictSlice(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from || taken.droppedSize == 0)
				Iterator.empty
			else {
				ff()
				underlying.slice(from, until)
			}

		override def slice(from :Int, until :Int) :Iterator[E] =
			if (until <= 0 | until <= from || taken.droppedSize == 0)
				Iterator.empty
			else if (underlying ne null)
				underlying.slice(from, until)
			else
				LazyIterator { ff(); underlying.slice(from, until) }

		override def splitAt(n :Int) :(Iterator[E], Iterator[E]) = {
			val size = taken.droppedSize
			if (n <= 0 | size == 0)
				(Iterator.empty, if (underlying ne null) underlying else this)
			else if (size >= 0 & n >= size)
				(if (underlying ne null) underlying else this, Iterator.empty)
			else
				Iterators.splitAt(LazyIterator { ff(); underlying }, n)
		}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 || start >= xs.length || taken.droppedSize == 0)
				0
			else if (start < 0)
				outOfBounds_!(
					toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len + ")"
				)
			else {
				ff()
				underlying.copyToArray(xs, start, len)
			}
		@inline private def ff() :Unit =
			if (underlying eq null)
				underlying = taken.finish()

		override def toString :String =
			if (underlying == null) "Iterators.Drop(" + taken + ")" else underlying.toString
	}


	private class Slicer[E](private[this] var underlying :Iterator[E])
		extends AbstractIterator[E] with SingleDrop[E]
	{ outer =>
		private[this] var i = 0
		private def index :Int = i
		override def hasFastDrop = HasFastSlice(underlying)

		override def knownSize :Int = underlying.knownSize
		override def hasNext :Boolean = underlying.hasNext
		override def next() :E = { i += 1; underlying.next() }

		override def drop(n :Int, strict :Boolean) :Iterator[E] = {
			if (n > 0) {
				underlying = Iterators.drop(underlying, n, strict)
				i += math.min(Int.MaxValue - i, n)
			}
			this
		}
		override def take(n :Int) :Iterator[E] =
			if (n < 0) Iterator.empty
			else if (n >= Int.MaxValue - n) new Take(Int.MaxValue)
			else new Take(i + n)

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (len <= 0 || start >= xs.length || !underlying.hasNext)
				0
			else if (start < 0)
				outOfBounds_!(
					toString + ".copyToArray(" + errorString(xs) + ", " + start + ", " + len
				)
			else {
				val max = math.min(len, xs.length - start)
				val (prefix, suffix) = underlying.splitAt(max)
				underlying = suffix
				val copied = prefix.copyToArray(xs, start, max)
				i += copied
				copied
			}

		override def toString = iterator.toString

		private class Take(private[this] var until :Int)
			extends AbstractIterator[E] with SingleDrop[E]
		{
			override def hasFastDrop = outer.hasFastDrop
			override def knownSize = {
				val remaining = until - index
				val total = outer.knownSize
				if (remaining <= 0) 0
				else if (total < 0) -1
				else math.min(remaining, total)
			}
			override def hasNext   = index < until && underlying.hasNext
			override def next()    =
				if (index >= until)
					noSuch_!(toString)
				else
					Slicer.this.next()

			override def drop(n :Int, strict :Boolean) :Iterator[E] = {
				val dropped = math.min(n, until - index)
				if (dropped > 0)
					outer.drop(dropped, strict)
				this
			}
			override def take(n :Int) :Iterator[E] = {
				val i = index
				if (n <= 0) until = i
				else if (n >= Int.MaxValue - i) until = Int.MaxValue
				else until = math.min(until, i + n)
				this
			}
			override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
				if (len <= 0 || index >= until || start >= xs.length)
					0
				else
					outer.copyToArray(xs, start, math.min(len, until - index))

			override def toString :String = underlying.toString + ".take(" + (until - index) + ")"
		}
	}

}
