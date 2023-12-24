package net.noresttherein.sugar.collections

import java.lang.{Math => math}
import java.util.ConcurrentModificationException

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, BufferedIterator, View, mutable}

import net.noresttherein.sugar.arrays.extensions.MutableArrayExtension
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, IteratorExtension, IteratorObjectExtension}
import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.numeric.extensions.BooleanExtension
import net.noresttherein.sugar.reflect.extensions.classNameMethods
import net.noresttherein.sugar.typist.casting.extensions.castingMethods
import net.noresttherein.sugar.funny.generic
import net.noresttherein.sugar.outOfBounds_!




//todo: Views with the same functionality
//todo: drop/take/copyToArray methods are untested!
/** Iterators implementing various extension methods from `extensions`. */
private object Iterators {

	//shared implementations of extensions methods from IterableExtension and IteratorExtension

	def single[E](elem :E) :Iterator[E] = new Single(elem)

	def double[E](first :E, second :E) :Iterator[E] = new Double(first, second)

	def scanLeft[E, A](self :Iterator[E], z :A, f :(A, E) => A) :Iterator[A] =
		new ScanLeft(self, z, f)

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

	def filterWith[E, A](self :Iterator[E], z :A, pred :(E, A) => (Boolean, A), keep :Boolean = true) :Iterator[E] =
		new FilterWith(self, z, pred, keep)

	def filterWithIndex[E](self :Iterator[E], pred :(E, Int) => Boolean, keep :Boolean = true) :Iterator[E] =
		new FilterWithIndex(self, pred, keep)

	def zipEven[E, X](self :Iterator[E], that :Iterator[X]) :Iterator[(E, X)] = new ZipEven(self, that)

	def zipMap[E, X, O](self :Iterator[E], that :Iterator[X])(f :(E, X) => O) :Iterator[O] =
		self.zip(that).map(f.tupled)

	def zipMapEven[E, X, O](self :Iterator[E], that :Iterator[X])(f :(E, X) => O) :Iterator[O] =
		new ZipEven(self, that).map(f.tupled)

	def zipMapAll[E, X, O](self :Iterator[E], that :Iterator[X], thisElem :E, thatElem :X)(f :(E, X) => O) :Iterator[O] =
		self.zipAll(that, thisElem, thatElem).map(f.tupled)

	def zipFlatMap[E, X, O](self :Iterator[E], that :Iterator[X])(f :(E, X) => IterableOnce[O]) :Iterator[O] =
		self.zip(that).flatMap(f.tupled)

	def zipFlatMapEven[E, X, O](self :Iterator[E], that :Iterator[X])(f :(E, X) => IterableOnce[O]) :Iterator[O] =
		new ZipEven(self, that).flatMap(f.tupled)

	def zipFlatMapAll[E, X, O](self :Iterator[E], that :Iterator[X], thisElem :E, thatElem :X)
	                          (f :(E, X) => IterableOnce[O]) :Iterator[O] =
		self.zipAll(that, thisElem, thatElem).flatMap(f.tupled)

	def zip3[E, A, B](self :Iterator[E], second :Iterator[A], third :Iterator[B]) :Iterator[(E, A, B)] =
		new Zip3(self, second, third)

	def zipEven3[E, A, B](self :Iterator[E], second :Iterator[A], third :Iterator[B]) :Iterator[(E, A, B)] =
		new ZipEven3(self, second, third)

	def zipAll3[E, A, B](self :Iterator[E], second :Iterator[A], third :Iterator[B],
	                     thisElem :E, secondElem :A, thirdElem :B) :Iterator[(E, A, B)] =
		new ZipAll3(self, thisElem, second, secondElem, third, thirdElem)

	def zipTail[E](self :Iterator[E]) :Iterator[(E, E)] =
		if (self.isEmpty) Iterator.empty
		else new ZipTail(self)

	def keep[E](self :Iterator[E], pred :Int => Boolean) :Iterator[E] =
		if (self.knownSize == 0) self else new Keep(self, pred, true)

	def distinct[E](self :Iterator[E]) :Iterator[E] = {
		val size = self.knownSize
		if (size >= 0 && size <= 1) self else new FirstOccurrences(self)
	}

	//All the following iterators could be simply replaced with
	def removed[E](self :Iterator[E], index :Int) :Iterator[E] =
		if (index < 0 || { val s = self.knownSize; s >= 0 & index >= s })
			throw new IndexOutOfBoundsException(index)
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
			val nonNegUntil = math.max(until, 0)
			if (size >= 0 && from >= size)
				self
			else
				new RemovedSlice(self, nonNegFrom, nonNegUntil - 1)
		}

	def updated[E](self :Iterator[E], index :Int, elem :E) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index >= size })
			throw new IndexOutOfBoundsException(self.toString + ".updated(" + index + ", _)")
		else
			new Updated(self, index, elem)

	def updatedAll[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] = {
		val size      = self.knownSize
		val patchSize = elems.knownSize
//		if (index < 0 || size >= 0 & patchSize >= 0 & index > size - patchSize)
//			throw new IndexOutOfBoundsException(
//				self.toString + (if (size >= 0) "|" + size + "|.updatedAll(" else ".updatedAll(") +
//					index + ", " + elems + ")"
//			)
//		if (patchSize == 0 | size >= 0 & index >= size | patchSize >= 0 & index <= 0 & index + patchSize <= 0)
//			self
		if (index < 0 || size >= 0 & index > (if (patchSize >= 0) size - patchSize else size))
			throw new IndexOutOfBoundsException(
				self.toString + (if (size >= 0) "|" + size + "|.updatedAll(" else ".updatedAll(") +
					index + ", " + elems + ")"
			)
//			else if (patchSize == 0) //we still need to validate the range, per contract
//				self
		if (patchSize == 0 && size >= 0)
			self
		else
			new UpdatedAll(self, index, elems.iterator)
	}

	//todo: permissive indexing. It is better to be consistent with patch,
	// and because the valid range in Ranking depends on whether the element is already in the collection.
	def inserted[E](self :Iterator[E], index :Int, elem :E) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index > size })
			throw new IndexOutOfBoundsException(self.toString + ".insertedAll(" + index.toString + ", " + elem + ")")
		else
			new Inserted(self, index, elem)

	//todo: permissive indexing
	def insertedAll[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] =
		if (index < 0 || { val size = self.knownSize; size >= 0 & index > size })
			throw new IndexOutOfBoundsException(self.toString + ".insertedAll(" + index.toString + ", _:*)")
		else
			new InsertedAll(self, index, elems.iterator)
	//		self.patch(index, elems.iterator, 0)

	def appended[E](self :Iterator[E], elem :E) :Iterator[E] = concat(self, Iterator.single(elem))

	def prepended[E](self :Iterator[E], elem :E) :Iterator[E] = concat(Iterator.single(elem), self)

	def concat[E](first :Iterator[E], second :Iterator[E]) :Iterator[E] =
		if (!first.hasNext)
			second
		else if (!second.hasNext)
			first
		else first match {
			case i :Concat[E] => i.append(second)
			case _ => second match {
				case i :Concat[E] => i.prepend(first)
				case _            => new Concat(first, second)
			}
		}

	/** An iterator with safe slicing methods. Invoking `take`, `drop`, `slice` does not invalidate this validator;
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
		final class Take(private[this] var underlying :Iterator[E], private[this] var limit :Int)
			extends AbstractBufferedIterator[E]
		{
			private[this] var lookahead :mutable.Queue[E] = _
			private[this] var i = 0

			private[Iterators] def droppedSize :Int =
				if (i >= idx | (lookahead ne null))
					-1
				else {
					val k = underlying.knownSize
					if (k < 0) -1
					else if (k <= idx - i) 0
					else k - idx + i
				}
			override def knownSize :Int =
				if (i >= limit)
					if (super.hasNext) 1 else -1
				else {
					val k = underlying.knownSize
					if (k < 0) -1 else math.min(k, limit - i) + super.hasNext.toInt
				}
			override def hasNext :Boolean = super.hasNext || i < limit && {
				if (lookahead ne null)
					lookahead.nonEmpty && { i += 1; push(lookahead.dequeue()) }
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
			override def drop(n :Int) :Iterator[E] = {
				if (n > 0 && i < limit) {
					var toDrop = math.min(n, limit - i)
					if (super.hasNext) {
						pop()
						toDrop -= 1
					}
					if (lookahead ne null)
						lookahead.dropInPlace(toDrop)
					else
						underlying = underlying.drop(toDrop)
				}
				this
			}
			override def slice(from :Int, until :Int) :Iterator[E] =
				if (until <= 0 | until <= from) Iterator.empty
				else if (from <= 0) take(until)
				else drop(from).take(until - from)

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
					if (lookahead ne null) {
						val copied = lookahead.copyToArray(xs, offset, max)
						lookahead.remove(0, copied)
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
						if (lookahead eq null)
							lookahead = new mutable.Queue[E]
						while (i < limit && underlying.hasNext) {
							lookahead += underlying.next()
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

		class Drop(taken :Take) extends AbstractIterator[E] {
			private[this] var underlying :Iterator[E] = _
			override def knownSize :Int =
				if (underlying == null) taken.droppedSize else underlying.knownSize

			override def hasNext :Boolean =
				(underlying eq null) && {
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
			override def drop(n :Int) :Iterator[E] =
				if (n <= 0 || taken.droppedSize == 0)
					this
				else {
					ff()
					underlying.drop(n)
				}
			override def take(n :Int) :Iterator[E] =
				if (n <= 0 || taken.droppedSize == 0)
					Iterator.empty
				else {
					ff()
					underlying.take(n)
				}
			override def slice(from :Int, until :Int) :Iterator[E] =
				if (until <= 0 | until <= from || taken.droppedSize == 0)
					Iterator.empty
				else {
					ff()
					underlying.slice(from, until)
				}
			override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
				if (n <= 0)
					(Iterator.empty, if (taken eq null) underlying else this)
				else if (taken.droppedSize == 0)
					SplitEmpty
				else {
					ff()
					Iterators.splitAt(underlying, n)
				}
			override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
				if (len <= 0 || start >= xs.length || taken.droppedSize == 0)
					0
				else if (start < 0)
					throw new IndexOutOfBoundsException(
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

		if (idx <= 0)
			(Iterator.empty, iter)
		else {
			val size = iter.knownSize
			if (size >= 0 & idx >= size)
				(iter, Iterator.empty)
			else {
				val take = new Take(iter, idx)
				(take, new Drop(take))
			}
		}
	}

	@tailrec def reverse[E](items :IterableOnce[E]) :Iterator[E] = items match {
		case indexed :collection.IndexedSeqOps[E, generic.Any, _] =>
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


	private class ReverseSortedSetIterator[+E](set :collection.SortedSet[E])
		extends KnownSizeVar[E](set.size)
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
		extends KnownSizeVar[(K, V)](map.size)
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



	private[Iterators] sealed abstract class KnownSizeVar[+E](private[this] var currentSize :Int)
		extends AbstractIterator[E] with IteratorSlicing[E]
	{
		final override def knownSize = currentSize
		final def knownSize_=(value :Int) :Unit = currentSize = value
		final def knownSize_--() :Unit = currentSize -= 1
		override def hasNext = currentSize > 0
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0 & currentSize >= 0)
				currentSize = math.max(0, currentSize - n)
			this
		}
		override def take(n :Int) :Iterator[E] = {
			if (n <= 0) {
				currentSize = 0; this
			} else if (currentSize >= n) {
				currentSize = n; this
			} else if (currentSize >= 0)
				this
			else
				super.take(n)
		}
	}

	final class Single[+E](hd :E) extends KnownSizeVar[E](1) {
		override def next() :E =
			if (hasNext) hd else throw new NoSuchElementException("Iterator.empty")

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
			val copied = util.elementsToCopy(xs, start, len, knownSize)
			if (copied > 0)
				xs(start) = hd
			copied
		}
		override def toString :String = if (hasNext) "Iterator(" + hd + ")" else "Iterator()"
	}

	final class Double[+E](first :E, second :E) extends KnownSizeVar[E](2) {
		override def next() :E = knownSize match {
			case 2 => knownSize = 1; first
			case 1 => knownSize = 0; second
			case _ => throw new NoSuchElementException("Iterator.empty")
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
			val copied = util.elementsToCopy(xs, start, len, size)
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

	final class ConstInfinite[+E](override val head :E) extends AbstractIterator[E] with  BufferedIterator[E] {
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

	final class Const[+E](initSize :Int, override val head :E)
		extends KnownSizeVar[E](initSize) with BufferedIterator[E]
	{
		override def next() :E = {
			val size = knownSize
			if (size <= 0)
				throw new NoSuchElementException("Iterators.Const(" + head + ")")
			knownSize -= 1; head
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0)
				knownSize = math.max(knownSize - n, 0)
			this
		}
		override def take(n :Int) :Iterator[E] = {
			knownSize = math.min(knownSize, math.max(n, 0))
			this
		}
		override def slice(from :Int, until :Int) :Iterator[E] = take(until).drop(from)

		override def splitAt(n :Int) :(Iterator[E], Iterator[E]) =
			if (n <= 0) (Iterator.empty, this)
			else if (n >= knownSize) (this, Iterator.empty)
			else (new Const(n, head), drop(n))

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
			val copied = util.elementsToCopy(xs, start, len, knownSize)
			xs.fill(start, start + copied)(head)
			copied
		}

		override def toString :String = "Iterator.const|" + knownSize + "|(" + head + ")"
	}


	private[collections] abstract class AbstractFlatMap[+Y] extends AbstractIterator[Y] with IteratorSlicing[Y] {
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
				throw new NoSuchElementException("empty iterator")
			val res = curr.next()
			currNonEmpty = curr.hasNext
			res
		}
	}


	private[collections] class MapWith[X, A, +Y](private[this] var underlying :Iterator[X],
	                                             private[this] var acc :A, f :(X, A) => (Y, A))
		extends AbstractIterator[Y]
	{
		override def knownSize :Int = underlying.knownSize

		override def hasNext :Boolean = underlying.hasNext
		override def next() :Y = { val res = f(underlying.next(), acc); acc = res._2; res._1 }

		override def toString :String = underlying.toString + ".mapWith(" + acc + ")"
	}
//
//	private[collections] class FlatMapWith[X, A, +Y](private[this] var underlying :Iterator[X],
//	                                                 private[this] var acc :A, f :(X, A) => (IterableOnce[Y], A))
//		extends AbstractFlatMap[Y]
//	{
//		override def hasNext :Boolean = super.hasNext || {
//			val (items, next) = f(underlying.next(), acc)
//			acc = next
//			enqueue(items.iterator)
//		}
//		override def toString = underlying.toString + ".flatMapWith(" + acc + ")"
//	}

	private[collections] class ScanLeft[X, +Y](private[this] var underlying :Iterator[X],
	                                           private[this] var acc :Y, f :(Y, X) => Y)
		extends AbstractBufferedIterator[Y]
	{
		push(acc)
		override def knownSize :Int = {
			val size = underlying.knownSize
			if (size < 0) -1 else size + super.hasNext.toInt
		}
		override def hasNext :Boolean = super.hasNext || underlying.hasNext && {
			acc = f(acc, underlying.next())
			push(acc)
		}
		override def toString :String = underlying.toString + ".scanLeft(->" + acc + ")"
	}


	private[Iterators] trait AbstractWithIndex[X, +Y] extends IteratorSlicing[Y] {
		protected def iter :Iterator[X]
		protected def dropped(n :Int, curr :Iterator[X]) :Unit

		override def knownSize :Int = iter.knownSize

		override def drop(n :Int) :Iterator[Y] = {
			if (n > 0)
				dropped(n, iter.drop(n))
			this
		}
		override def take(n :Int) :Iterator[Y] =
			if (n < 0)
				Iterator.empty
			else {
				dropped(0, iter.take(n))
				this
			}
		override def slice(from :Int, until :Int) :Iterator[Y] = take(until).drop(from)
	}

	final class MapWithIndex[X, +Y](private[this] var underlying :Iterator[X], f :(X, Int) => Y)
		extends AbstractIterator[Y] with AbstractWithIndex[X, Y]
	{
		private[this] var i :Int = -1
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Y = { i += 1; f(underlying.next(), i) }

		protected override def iter :Iterator[X] = underlying
		protected override def dropped(n :Int, curr :Iterator[X]) :Unit = { i += n; underlying = curr }
		override def toString :String = underlying.toString + ".mapWithIndex(@" + i + ")"
	}
//
//	final class FlatMapWithIndex[X, +Y](private[this] var source :Iterator[X], f :(X, Int) => IterableOnce[Y])
//		extends AbstractFlatMap[Y]
//	{
//		private[this] var i = -1
//		override def hasNext :Boolean = {
//			var nonEmpty = super.hasNext
//			while (!nonEmpty && source.hasNext) {
//				i += 1
//				val items = f(source.next(), i)
//				val size =  items.knownSize
//				nonEmpty = size != 0 && enqueue(items.iterator)
//			}
//			nonEmpty
//		}
//		override def toString :String = source.toString + ".flatMapWithIndex(@" + i + ")"
//	}

	final class CollectWithIndex[X, Y](private[this] var source: Iterator[X], f: PartialFunction[(X, Int), Y])
		extends AbstractBufferedIterator[Y]
	{
		private[this] val fallback = Iterators.fallback.asInstanceOf[((X, Int)) => Y]
		private[this] var i = -1
		override def hasNext :Boolean = super.hasNext || {
			var found = false
			while(!found && source.hasNext) {
				i += 1
				val next = f.applyOrElse((source.next(), i), fallback)
				if (next.asAnyRef ne fallback) {
					push(next)
					found = true
				}
			}
			found
		}
		override def toString :String = source.toString + ".collectWithIndex@" + i + "(" + f + ")"
	}
	private val fallback :Any => Any = _ => fallback


	final class MapWhile[X, S, +Y](source :Iterator[X], private[this] var state :S,
	                               pred :S => Boolean, f :(S, X) => (S, Y))
		extends AbstractIterator[Y] with IteratorSlicing[Y]
	{
		override def hasNext :Boolean = source.hasNext && pred(state)
		override def next() :Y = {
			val (a, res) = f(state, source.next())
			state = a
			res
		}
		override def toString :String = source.toString + ".mapWhile(" + state + ")"
	}
//	final class FlatMapWhile[X, A, +Y](source :Iterator[X], private[this] var state :A,
//	                                   pred :A => Boolean, f :(A, X) => (A, IterableOnce[Y]))
//		extends AbstractFlatMap[Y]
//	{
//		override def hasNext :Boolean = {
//			var nonEmpty = super.hasNext
//			while (!nonEmpty && source.hasNext && pred(state)) {
//				val (a, items) = f(state, source.next())
//				state = a
//				val size  = items.knownSize
//				nonEmpty = size != 0 && enqueue(items.iterator)
//			}
//			nonEmpty
//		}
//		override def toString :String = source.toString + ".flatMapWhile(" + state + ")"
//	}

	final class MapUntil[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => (Boolean, S, Y))
		extends AbstractBufferedIterator[Y]
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

	final class MapSome[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => Option[(S, Y)])
		extends AbstractIterator[Y] with IteratorSlicing[Y]
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
			case null => source.toString + ".mapSome(" + state + ")"
			case Some((s, e)) => source.toString + ".mapSome(" + s + "->" + e + ")"
			case _ => "Iterator()"
		}
	}
//	final class FlatMapSome[X, S, +Y](source: Iterator[X], private[this] var state: S,
//	                                  f: (S, X) => Option[(S, IterableOnce[Y])])
//		extends AbstractFlatMap[Y]
//	{
//		private[this] var hd :Option[(S, IterableOnce[Y])] = _
//
//		override def hasNext :Boolean = {
//			var nonEmpty = super.hasNext
//			while (!nonEmpty && hd == null && source.hasNext) {
//				hd = f(state, source.next())
//				hd match {
//					case Some((a, items)) =>
//						state = a
//						val size = items.knownSize
//						hd = null
//						nonEmpty = size != 0 && enqueue(items.iterator)
//					case _ =>
//				}
//			}
//			nonEmpty
//		}
//		override def toString :String = hd match {
//			case null => source.toString + ".flatMapSome(" + state + ")"
//			case Some((s, e)) => source.toString + ".flatMapSome(" + s + "->" + e + ")"
//			case _ => "Iterator()"
//		}
//	}


	final class FilterWith[E, S](underlying :Iterator[E], private[this] var state :S,
	                             f :(E, S) => (Boolean, S), keep :Boolean = true)
		extends AbstractBufferedIterator[E]
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

	final class FilterWithIndex[E](underlying :Iterator[E], f :(E, Int) => Boolean, keep :Boolean = true)
		extends AbstractBufferedIterator[E]
	{
		private[this] var i = -1

		override def hasNext :Boolean = super.hasNext || {
			while (underlying.hasNext) {
				val peek = underlying.next()
				i += 1
				if (f(peek, i) == keep) {
					return push(peek)
				}
			}
			false
		}
		override def toString :String =
			if (super.hasNext)
			//consider: a new toString implementation for everything; i does not reflect the actual index of this iterator.
				"Iterator@" + (i + 1) + "(" + head + ",...)"
			else {
				val name = if (keep) ".filterWithIndex@" else ".filterNotWithIndex@"
				underlying.toString + name + (i + 1)
			}
	}


	//Validation of even length hurts performance
	final class ZipEven[+A, +B](private[this] var i1 :Iterator[A], private[this] var i2 :Iterator[B])
		extends AbstractIterator[(A, B)] with IteratorSlicing[(A, B)]
	{
		{
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			if (size1 >= 0 & size2 >= 0 & size1 != size2)
				throw new NoSuchElementException(
					i1.toString + " is of size " + size1 + ", while " + i2 + " is of size " + size2
				)
		}
		override def knownSize :Int = {
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			if (size1 == size2) size1
			else -1
		}
		override def hasNext :Boolean = {
			val has1 = i1.hasNext
			val has2 = i2.hasNext
			if (has1 != has2)
				throw new NoSuchElementException("Sizes of iterators differ")
			has1
		}
		override def next() :(A, B) = (i1.next(), i2.next())

		override def take(n :Int) :Iterator[(A, B)] = {
			i1 = i1.take(n)
			i2 = i2.take(n)
			this
		}

		override def toString :String= "(" + i1 + " zipEven " + i2 + ")"
	}

	final class ZipEven3[+A, +B, +C]
	            (private[this] var i1 :Iterator[A], private[this] var i2 :Iterator[B], private[this] var i3 :Iterator[C])
		extends AbstractIterator[(A, B, C)] with IteratorSlicing[(A, B, C)]
	{
		{
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			val size3 = i3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0 & (size1 != size2 | size2 != size3))
				throw new NoSuchElementException(
					"Iterators " + i1 + ", " + i2 + ", " + i3 + " have different sizes: " +
						size1 + ", " + size2 + ", " + size3 + "."
				)
		}
		override def knownSize :Int = {
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			val size3 = i3.knownSize
			if (size1 == size2 & size2 == size3) size1
			else -1
		}
		override def hasNext :Boolean = {
			val has1 = i1.hasNext
			val has2 = i2.hasNext
			val has3 = i3.hasNext
			if (has1 != has2 | has2 != has3)
				throw new NoSuchElementException("Sizes of iterators differ")
			has1
		}
		override def next() :(A, B, C) = (i1.next(), i2.next(), i3.next())

		override def take(n :Int) :Iterator[(A, B, C)] = {
			i1 = i1.take(n)
			i2 = i2.take(n)
			i3 = i3.take(n)
			this
		}
		override def toString :String= i1.toString + ".zipEven(" + i2 + ", " + i3 + ")"
	}


	final class Zip3[+A, +B, +C]
	            (private[this] var i1 :Iterator[A], private[this] var i2 :Iterator[B], private[this] var i3 :Iterator[C])
		extends AbstractIterator[(A, B, C)] with IteratorSlicing[(A, B, C)]
	{
		override def knownSize :Int = {
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			val size3 = i3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0)
				math.min(size1, math.min(size2, size3))
			else
				-1
		}
		override def hasNext :Boolean = i1.hasNext && i2.hasNext && i3.hasNext
		override def next() :(A, B, C) = (i1.next(), i2.next(), i3.next())

		override def drop(n :Int) :Iterator[(A, B, C)] = {
			i1 = i1.drop(n)
			i2 = i2.drop(n)
			i3 = i3.drop(n)
			this
		}
		override def take(n :Int) :Iterator[(A, B, C)] = {
			i1 = i1.take(n)
			i2 = i2.take(n)
			i3 = i3.take(n)
			this
		}
		override def toString :String = "Iterator.zip3(" + i1 + ", " + i2 + ", " + i3 + ")"
	}

	final class ZipAll3[+A, +B, +C](
		private[this] var i1 :Iterator[A], pad1 :A,
		private[this] var i2 :Iterator[B], pad2 :B,
		private[this] var i3 :Iterator[C], pad3 :C
	)   extends AbstractIterator[(A, B, C)] with IteratorSlicing[(A, B, C)]
	{
		override def knownSize :Int = {
			val size1 = i1.knownSize
			val size2 = i2.knownSize
			val size3 = i3.knownSize
			if (size1 >= 0 & size2 >= 0 & size3 >= 0)
				math.max(size1, math.max(size2, size3))
			else
				-1
		}
		override def hasNext :Boolean = i1.hasNext || i2.hasNext || i3.hasNext

		override def next() :(A, B, C) = {
			val a = if (i1.hasNext) i1.next() else pad1
			val b = if (i2.hasNext) i2.next() else pad2
			val c = if (i3.hasNext) i3.next() else pad3
			(a, b, c)
		}
		override def drop(n :Int) :Iterator[(A, B, C)] = {
			i1 = i1.drop(n)
			i2 = i2.drop(n)
			i3 = i3.drop(n)
			this
		}
		override def take(n :Int) :Iterator[(A, B, C)] = {
			i1 = i1.take(n)
			i2 = i2.take(n)
			i3 = i3.take(n)
			this
		}
		override def toString :String =
			"Iterator.zipAll3(" + i1 + "++" + pad1 + "*, " + i2 + "++" + pad2 + "*, "  + i3 + "++" + pad3 + "*)"
	}

	final class ZipTail[+E](private[this] var underlying :Iterator[E])
		extends AbstractIterator[(E, E)] with IteratorSlicing[(E, E)]
	{
		private[this] var prev = underlying.next()
		override def knownSize :Int = underlying.knownSize
		override def hasNext   :Boolean = underlying.hasNext

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
		override def drop(n :Int) :Iterator[(E, E)] = {
			if (n > 0) {
				if (n > 1)
					underlying = underlying.drop(n - 1)
				if (underlying.hasNext)
					prev = underlying.next()
			}
			this
		}
		override def toString :String = underlying.toString + ".zipTail"
	}


	final class FirstOccurrences[+E](private[this] var underlying :Iterator[E])
		extends AbstractBufferedIterator[E]
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


	final class Keep[+E](underlying :Iterator[E], test :Int => Boolean, expect :Boolean)
		extends AbstractBufferedIterator[E]
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


	final class Removed[+E](private[this] var underlying :Iterator[E], index :Int,
	                        private[this] var validating :Boolean = true)
		extends AbstractBufferedIterator[E]
	{   //todo: don't extend AbstractBufferedIterator, it only complicates the implementation.
		//The number of consumed items from underlying, not the number of consumed items from this.
		// Counting stops after passing index in order not to overflow.
		private[this] var i = 0
		def offset :Int = i

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
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else  {
				var toDrop = n
				if (super.hasNext) {        //essentially, we need to reduce n by 1
					toDrop -= 1
					pop()
				}
				if (i > index)               //Unwrap ourselves, as we are past the removed index
					underlying.drop(toDrop)
				else if (toDrop < index - i) {
					underlying = underlying.drop(toDrop)
					i += n
					this
				} else { //We need to drop past the removed index. First drop up until index to validate, then drop the rest.
					val dropAfterIndex = toDrop - index + i
					underlying = underlying.drop(index - i)
					i = index
					if (hasNext)                 //hasNext will throw an exception if index is out of range.
						drop(dropAfterIndex - 1) //hasNext already has dropped the first (that is, index-th) element.
					else
						this
				}
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
					else {
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
		private def ioob() = throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + i)

		override def toString :String =
			if (super.hasNext)
				"Iterator@" + i + "(" + head + ",...)"
			else
				underlying.toString + "@" + i + ".removed(" + index + ")"
	}

	//An overflow conscious implementation. However, untilInclusive == Int.MaxValue is de facto treated as infinity.
	final class RemovedSlice[+E](private[this] var underlying :Iterator[E], from :Int, untilInclusive :Int)
		extends AbstractBufferedIterator[E]
	{
		private[this] var i = 0
		def offset :Int = i

		override def knownSize :Int = {
			val size = underlying.knownSize
			if (size < 0)
				-1
			else if (i > untilInclusive)
				size
			else {
				val start = math.min(from, size)
				val end   = math.min(math.max(untilInclusive, start - 1), size - 1)
				size - (end - start) - 1
			}
		}
		override def hasNext :Boolean = super.hasNext || {
			if (i == from) {
				underlying = underlying.drop(untilInclusive - from + 1)
				i = untilInclusive + 1
			}
			underlying.hasNext && {
				i += 1
				push(underlying.next())
			}
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0) {
				var toDrop = n
				if (super.hasNext) {
					toDrop += 1
					pop()
				}
				if (i > untilInclusive)
					underlying = underlying.drop(toDrop) //don't increase i to avoid overflow
				else if (toDrop < from - i) {
					i += n
					underlying = underlying.drop(n)
				} else {
					underlying = underlying.drop(untilInclusive - i)
					i = untilInclusive
					pop()
				}
			}
			this
		}
		override def take(n :Int) :Iterator[E] = {
			underlying = underlying.take(n)
			this
		}

		override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int =
			if (i > untilInclusive)
				underlying.copyToArray(xs, start, len)
			else if (len <= 0 | from == 0 && untilInclusive == Int.MaxValue || start >= xs.length) //overflow guard
				0
			else if (i >= from && i <= untilInclusive) {
				if (untilInclusive == Int.MaxValue && i == 0)
					0
				else {
					underlying = underlying.drop(untilInclusive + 1 - i)
					underlying.copyToArray(xs, start, len)
				}
			} else {
				val max = math.min(len, xs.length - start)
				if (max <= from - i)
					underlying.copyToArray(xs, start, max)
				else if (max <= untilInclusive + 1 - i || untilInclusive == Int.MaxValue && i == 0)
					underlying.copyToArray(xs, start, from - i)
				else {
					val (before, after) = underlying.splitAt(from - i)
					val copiedBefore    = before.copyToArray(xs, start, from - i)
					underlying          = after.drop(untilInclusive + 1 - from)
					copiedBefore + underlying.copyToArray(xs, start + copiedBefore, max - copiedBefore)
				}
			}

		//todo: copyToArray
		override def toString :String =
			if (super.hasNext)
				"Iterator@" + i + "(" + head + ",...)"
//			else if (from == untilInclusive)
//				underlying.toString + "@" + i + ".removed(" + from + ")"
			else
				underlying.toString + "@" + i + ".removed(" + from + ".." + (untilInclusive + 1) + ")"
	}

	final class Updated[+E](private[this] var underlying :Iterator[E], index :Int, elem :E)
		extends AbstractIterator[E] with IteratorSlicing[E]
	{
		private[this] var i = 0
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
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else if (i > index)                 //Unwrap ourselves
				underlying.drop(n)
			else if (n <= index - i) {
				underlying = underlying.drop(n)
				i += n
				this
			} else {                            //Dropping past the updated index
				drop(index - i)
				hasNext                         //Validates and throws an IndexOutOfBoundsException if no more elements.
				underlying.drop(n - index + i)  //Now that we are past index, we can unwrap ourselves
			}
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
						throw new IndexOutOfBoundsException(
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
		private def ioob() = throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + ".updated(@" + (index - i) + "=" + elem + ")"
	}

	final class UpdatedAll[+E](private[this] var underlying :Iterator[E], index :Int, private[this] var elems :Iterator[E])
		extends AbstractIterator[E] with IteratorSlicing[E]
	{
		if (index < 0)
			throw new IndexOutOfBoundsException(toString)

		private[this] var i = 0

		override def knownSize :Int = {
			val size      = underlying.knownSize
			val elemsSize = elems.knownSize
			//If knownSize == 0 then IterableFactory.from will usually return an empty collection without iterating
			if (size >= 0 && index + size < i + math.max(elemsSize, 0))
				throw new IndexOutOfBoundsException("Iterator.empty.updatedAll(@" + index + "=" + elems + ")")
			else if (elemsSize < 0)
				-1
			else
				size
		}

		override def hasNext :Boolean =
			underlying.hasNext || {
				if (index > i || elems.hasNext) //updateAll(length, Nil) is allowed
					throw new IndexOutOfBoundsException(toString)
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
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else if (i >= index && !elems.hasNext)
				underlying.drop(n)
			else if (i >= index) {
				val remaining = elems.knownSize
				if (remaining > n) { //We can drop on both iterators without disabling validation.
					elems = elems.drop(n)
					underlying = underlying.drop(n)
					this
				} else if (remaining < 0)
					super.drop(n)    //Slow drop so we don't switch off validation.
				else if (remaining == 0)
					underlying.drop(n)
				else {               //Drop up to the last updated element to see if index is not out of range for underlying.
					elems = elems.drop(remaining - 1)
					underlying = underlying.drop(remaining - 1)
					if (!underlying.hasNext)
						throw new IndexOutOfBoundsException(
							toString + ": " + (index + remaining - 1) + " out of between " + i + " and " + (i + remaining - 1)
						)
					underlying.drop(n - remaining + 1)
				}
			} else if (n <= index - i) {
				underlying = underlying.drop(n)
				i += n
				this
			} else {                 //Reduce to the previous case.
				underlying = underlying.drop(index - i)
				i = index
				drop(n - index + i)
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
		private def ioob() = throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + ".updatedAll(@" + (index - i) + "=" + elems + ")"
	}


	final class Inserted[+E](private[this] var underlying :Iterator[E], index :Int, elem :E)
		extends AbstractIterator[E] with IteratorSlicing[E]
	{
		private[this] var i = 0

		override def knownSize :Int = {
			val k = underlying.knownSize
			if (k < 0) -1
			else if (k < index - i) throw new IndexOutOfBoundsException(index.toString + " out of " + (i + k))
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
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else if (i > index)
				underlying.drop(n)
			else if (n < index - i) {
				i += n
				underlying = underlying.drop(n)
				this
			} else {
				underlying = underlying.drop(index - i - 1)
				if (!underlying.hasNext)
					throw new IndexOutOfBoundsException(index.toString + " out of between " + i + " and " + (index - 1))
				underlying.next()
				if (n == index - i) {
					i = index
					this
				} else
					underlying.drop(n - index + i - 1)
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
		private def ioob() = throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + i)
		override def toString :String = underlying.toString + "@" + i + ".inserted(" + index + ", " + elem + ")"
	}

	//todo: make indexing permissive
	final class InsertedAll[+E](private[this] var underlying :Iterator[E], index :Int, private[this] var elems :Iterator[E])
		extends AbstractIterator[E] with IteratorSlicing[E]
	{
		private[this] var i = 0
		override def knownSize :Int = {
			val remaining = underlying.knownSize
			if (remaining >= 0 & remaining < index - i)
				throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + (i + remaining))
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

		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else if (i >= index && !elems.hasNext)
				underlying.drop(n)
			else if (i >= index) {
				val remaining = elems.knownSize
				if (remaining >= n) {
					elems = elems.drop(n)
					this
				} else if (remaining < 0) {
					var dropped = 0
					while (dropped < n && elems.hasNext) {
						elems.next()
						dropped += 1
					}
					if (!elems.hasNext)
						underlying.drop(n - dropped)
					else
						this
				} else {
					elems = Iterator.empty
					underlying.drop(n - remaining)
				}
			} else if (n < index - i) {
				underlying = underlying.drop(n)
				i += n
				this
			} else {
				underlying = underlying.drop(index - i - 1)
				if (!underlying.hasNext)
					throw new IndexOutOfBoundsException(
						toString + ": " + index + " out of between " + i + " and " + (index - 1)
					)
				underlying.next()
				val leftToDrop = n - index + 1
				i = index
				drop(leftToDrop)
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
							throw new IndexOutOfBoundsException(
								toString + ": " + index + " out of between " + (i + before)
							)
						elems.copyToArray(xs, start + before, max + before)
					} else {
						val (before, after) = underlying.splitAt(index - i)
						val copied = before.copyToArray(xs, start, len)
						if (copied < index - i)
							throw new IndexOutOfBoundsException(
								toString + ": " + index + " out of between " + (i + copied)
							)
						underlying = after
						i = index
						copied + copyToArray(xs, start + copied, max - copied)
					}
				}
			}

		private def ioob() = throw new IndexOutOfBoundsException(toString + ": " + index + " out of " + i)

		override def toString :String = {
			val elemsSize = elems.knownSize
			val elemsString = if (elemsSize >= 0) elems.className +  "|" + elemsSize + "|" else elems.className
			underlying.toString + "@" + i + ".insertedAll(" + index + ", " +  elemsString + ")"
		}
	}


	//consider: integrating the idea behind PassedArray and including the array directly.
	//Tempting to have iterators mutable, but either we'd need to copy everything on concat or throw away the original iterator.
	final class Concat[+E](private[this] var iterators :PassedArray[Iterator[E]])
		extends AbstractFlatMap[E]
	{
//		def this(iterators :PassedArray[Iterator[E]]) = this(iterators.head, iterators.tail)
		def this(first :Iterator[E], second :Iterator[E]) = this(PassedArray.two(first, second))

		private def list :PassedArray[Iterator[E]] = iterators
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

		override def drop(n :Int) :Iterator[E] =
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
				if (i == count)
					if (rem >= 0)
						Iterator.empty
					else {
						val last = iterators(count - 1)
						last.drop(last.knownSize + rem)
					}
				else if (rem == 0) {
					iterators = iterators.drop(i)
					enqueue(iterators(0))
					this
				} else if (rem < 0) {
					iterators = iterators.drop(i - 1)
					val head  = iterators(0)
					val dropped = head.drop(head.knownSize + rem)
					if (dropped ne head)
						iterators = dropped +: iterators.tail
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

/*
	final class Prepended[+E](private[this] var hd :E, private[this] var source :Iterator[E])
		extends HasNextVar[E](true) with BufferedIterator[E]
	{
		override def head :E = hd

		override def knownSize :Int = {
			val size = source.knownSize
			if (size >= 0) size + 1 else -1
		}
		override def next() :E = {
			val res = hd
			if (source.hasNext)
				hd = source.next()
			else
				hasNext = false
			res
		}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				source = source.drop(n - 1)
				if (source.hasNext)
					hd = source.next()
				else
					hasNext = false
				this
			}
		override def toString :String = hd.toString + " +: " + source
	}

	final class Appended[+E](private[this] var first :Iterator[E], private[this] var last :E)
		extends HasNextVar[E](true)
	{
		override def knownSize :Int =
			if (!hasNext)
				0
			else {
				val size = first.knownSize
				if (size >= 0) size + 1 else -1
			}

		override def next() :E =
			if (first.hasNext)
				first.next()
			else {
				hasNext = false
				last
			}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else first.knownSize match {
				case -1 =>
					super.drop(n)
				case k  if k >= n =>
					first = first.drop(n)
					this
				case _ =>
					first = Iterator.empty
					hasNext = false
					this
			}
		override def toString :String = first.toString + " :+ " + last
	}
*/

	private class Slicer[+E](private[this] var underlying :Iterator[E])
		extends AbstractIterator[E] with IteratorSlicing[E]
	{ outer =>
		private[this] var i = 0
		private[this] var taken = 0
		private def index :Int = i
		private def taken_--() :Unit = taken -= 1
		override def knownSize :Int = underlying.knownSize
		override def hasNext :Boolean = underlying.hasNext
		override def next() :E = { i += 1; underlying.next() }

		override def drop(n :Int) :Iterator[E] = {
			underlying = underlying.drop(n)
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
				throw new IndexOutOfBoundsException(
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
			extends AbstractIterator[E] with IteratorSlicing[E]
		{
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
					throw new NoSuchElementException(toString)
				else
					Slicer.this.next()

			override def drop(n :Int) :Iterator[E] = {
				val dropped = math.min(n, until - index)
				if (dropped > 0)
					outer.drop(dropped)
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



	private val SplitEmpty :(Iterator[Nothing], Iterator[Nothing]) = (Iterator.empty, Iterator.empty)
}





private[collections] trait IteratorSlicing[+E] extends Iterator[E] {
	override def slice(from :Int, until :Int) :Iterator[E] =
		if (until <= 0 | until <= from) Iterator.empty[E]
		else if (from <= 0) take(until)
		else drop(from).take(until - from)

	override def splitAt(n :Int) :(Iterator[E], Iterator[E]) = Iterators.splitAt(this, n)
}






/** A skeleton implementation for buffered iterators, typically derived from other iterators, for whom
  * computing the next element is either not trivial, or consumes the next value from a producer, and thus needs
  * to avoid duplication between `hasNext` and `next`. Most importantly, only
  * [[net.noresttherein.sugar.collections.AbstractBufferedIterator.hasNext hasNext]] method needs overriding,
  * which is at the same time responsible for precomputing and initializing the next value with `push(head)`.
  * Both `head` and `next()` always start with ensuring the next item is available, and, if so,
  * proceed to return the current `head`, or throw a [[NoSuchElementException]] otherwise.
  * The latter method additionally clears the head value before returning.
  * This may be also done manually by the subclass by the use of `pop()`.
  */
abstract class AbstractBufferedIterator[+Y]
	extends AbstractIterator[Y] with BufferedIterator[Y] with IteratorSlicing[Y]
{
	private[this] var knownNonEmpty = false
	private[this] var hd :Y         = _

	/** Delegates to [[net.noresttherein.sugar.collections.AbstractBufferedIterator.hasNext hasNext]] to ensure
	  * that the next item is stored in the iterator, end either returns it, or throws a [[NoSuchElementException]],
	  * depending on the returned value.
	  */
	override def head :Y =
		if (knownNonEmpty || hasNext) hd
		else throw new NoSuchElementException("empty iterator")

	protected final def last :Y = hd

	/** Sets the `head` value of the iterator, and `hasNext` to `true`.
	  * This method is typically called by [[net.noresttherein.sugar.collections.AbstractBufferedIterator.hasNext hasNext]]
	  * before it returns `true`. Calling those methods has an additional effect in that `hasNext` implementation
	  * defined here will return `true`. As `push` is typically called in the implementation of `hasNext`
	  * in the case when the latter should return `true`, this method always return `true` for the convenience
	  * of being able to return `push(elem)` in those cases.
	  * @see [[net.noresttherein.sugar.collections.AbstractBufferedIterator.pop pop]]
	  */
	protected def push(head :Y @uncheckedVariance) :Boolean = {
		hd = head
		knownNonEmpty = true
		true
	}

	/** Invalidates the currently stored `head`, if any. Dropping an existing value has the effect of advancing
	  * iterator, as any future call to `head`, `next`, or `hasNext` will precompute the next value.
	  */
	protected def pop() :Unit = knownNonEmpty = false

	/** True, if the next element is currently buffered on the `hd` variable. Must be overridden by subclasses
	  * to advance/attempt to advance the iterator when `false`. Overriding method should always first defer
	  * to super in order to check if a `head` value is already buffered. Overriding implementation for a filtering
	  * iterator would thus look something like:
	  * {{{
	  *     override def hasNext =
	  *         super.hasNext || {
	  *             while(underlying.hasNext) {
	  *                 val e = underlying.next()
	  *                 if (pred(e)) {
	  *                     push(e)
	  *                     return true
	  *                 }
	  *             }
	  *             false
	  *         }
	  * }}}
	  * This is sufficient for a functioning iterator implementation.
	  */
	override def hasNext :Boolean = knownNonEmpty

	/** If `head` is already cached, return it and set `hasNext` to false. Otherwise defer to `hasNext` overridden
	  * by the subclass for an authoritative answer which advances the iterator, and either return the, now cached,
	  * item, or throw a [[NoSuchElementException]] if the method returned `false`.
	  */
	override def next() :Y = {
		if (!knownNonEmpty && !hasNext)
			throw new NoSuchElementException("empty iterator")
		knownNonEmpty = false
		hd
	}

	override def toString :String =
		this.localClassName + "(" + (if (knownNonEmpty) head.toString + ", ...)" else "<not computed>)")
}






/** A proxy iterator maintaining a counter of returned (or skipped) elements. */
class CountingIterator[+E](private[this] var underlying :Iterator[E], private[this] var counter :Int = 0)
	extends Iterator[E]
{
	def total :Int = counter
	override def hasNext :Boolean = underlying.hasNext
	override def next() :E = { val res = underlying.next(); counter += 1; res }

	override def drop(n :Int) :CountingIterator[E] = {
		if (n > 0) {
			val size = underlying.knownSize
			if (size >= 0) {
				counter += math.min(size, n)
				underlying.drop(n)
			} else {
				var remaining = n
				while (remaining > 0 && underlying.hasNext) {
					underlying.next()
					remaining -= 1
				}
				counter += n - remaining
			}
		}
		this
	}
	override def take(n :Int) :CountingIterator[E] = {
		underlying = underlying.take(n)
		this
	}
	override def slice(from :Int, until :Int) :CountingIterator[E] =
		if (from <= 0)
			take(until)
		else if (until <= 0)
			drop(from).take(0)
		else
			drop(from).take(until - from)

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = underlying.copyToArray(xs, start, len)
		counter += copied
		copied
	}

	override def toString :String = "CountingIterator(" + counter + ": " + underlying + ")"
}
