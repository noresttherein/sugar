package net.noresttherein.sugar.collections

import java.lang.{Math => math}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.extensions.{BooleanExtension, castingMethods, classNameMethods}
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, IteratorExtension, IteratorObjectExtension}
import net.noresttherein.sugar.outOfBounds_!



/** Iterators implementing various extension methods from `extensions`. */
private object Iterators {

	//shared implementations of extensions methods from IterableExtension and IteratorExtension

	def mapWith[E, O, A](self :Iterator[E], z :A, f :(E, A) => (O, A)) :Iterator[O] =
		//safe because null is never passed to f and we are in an erased context
		self.scanLeft((null.asInstanceOf[O], z)) { (acc, e) => f(e, acc._2) }.drop(1).map(_._1)

	def flatMapWith[E, O, A](self :Iterator[E], z :A, f :(E, A) => (IterableOnce[O], A)) :Iterator[O] =
		self.scanLeft((Nil :IterableOnce[O], z)) {
			(acc, e) => f(e, acc._2)
		}.drop(1).flatMap(_._1)

	def mapWithIndex[E, O](self :Iterator[E], f :(E, Int) => O) :Iterator[O] = new MapWithIndex(self, f)

	def flatMapWithIndex[E, O](self :Iterator[E], f :(E, Int) => IterableOnce[O]) :Iterator[O] =
		new FlatMapWithIndex(self, f)

	def collectWithIndex[E, O](self :Iterator[E], f :PartialFunction[(E, Int), O]) :Iterator[O] =
		new CollectWithIndex(self, f)

	def mapWhile[E, O, A](self :Iterator[E], z :A, pred :A => Boolean, f :(A, E) => (A, O)) :Iterator[O] =
		new MapWhile(self, z, pred, f)

	def flatMapWhile[E, O, A](self :Iterator[E], z :A, pred :A => Boolean, f :(A, E) => (A, IterableOnce[O]))
			:Iterator[O] =
		new FlatMapWhile(self, z, pred, f)

	def mapUntil[E, A, O](self :Iterator[E], z :A, f :(A, E) => (Boolean, A, O)) :Iterator[O] =
		self.scanLeft((true, z, null.asInstanceOf[O])) { (acc, e) => f(acc._2, e) }.drop(1).takeWhile(!_._1).map(_._3)

	def flatMapUntil[E, A, O](self :Iterator[E], z :A, f :(A, E) => (Boolean, A, IterableOnce[O])) :Iterator[O] =
		self.scanLeft((true, z, Nil :IterableOnce[O])) {
			(acc, e) => f(acc._2, e)
		}.drop(1).takeWhile(!_._1).flatMap(_._3)

	def mapSome[E, A, O](self :Iterator[E], z :A, f :(A, E) => Option[(A, O)]) :Iterator[O] =
		new MapSome(self, z, f)

	def flatMapSome[E, A, O](self :Iterator[E], z :A, f :(A, E) => Option[(A, IterableOnce[O])]) :Iterator[O] =
		new FlatMapSome(self, z, f)

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

	def removed[E](self :Iterator[E], index :Int) :Iterator[E] =
		if (index < 0)
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
//				self.toString + (if (size >= 0) "<" + size + ">.updatedAll(" else ".updatedAll(") +
//					index + ", " + elems + ")"
//			)
		if (patchSize == 0 | size >= 0 & index >= size | patchSize >= 0 & index <= 0 & index + patchSize <= 0)
			self
		else
			new UpdatedAll(self, index, elems.iterator)
	}

	def inserted[E](self :Iterator[E], index :Int, elem :E) :Iterator[E] =
		if (index < 0)
			throw new IndexOutOfBoundsException(self.toString + ".insertedAll(" + index.toString + ", " + elem + ")")
		else
			new Inserted(self, index, elem)

	def insertedAll[E](self :Iterator[E], index :Int, elems :IterableOnce[E]) :Iterator[E] =
		if (index < 0)
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



	private[Iterators] sealed abstract class KnownSizeVar[+A](private[this] var currentSize :Int)
		extends AbstractIterator[A]
	{
		final override def knownSize = currentSize
		final def knownSize_=(value :Int) :Unit = currentSize = value
		override def hasNext = currentSize > 0
		override def drop(n :Int) :Iterator[A] = {
			if (n > 0 & currentSize >= 0)
				currentSize = math.max(0, currentSize - n)
			this
		}
		override def take(n :Int) :Iterator[A] = {
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
	private[Iterators] sealed abstract class HasNextVar[+A](private[this] var nonEmpty :Boolean)
		extends AbstractIterator[A]
	{
		final override def hasNext :Boolean = nonEmpty
		final def hasNext_=(value :Boolean) :Unit = nonEmpty = value
	}

//	abstract class AbstractMappedIterator[X, +Y](underlying :Iterator[X]) extends Iterator[Y] {
//		override def knownSize :Int = underlying.knownSize
//		override def hasNext   :Boolean = underlying.hasNext
//
//		override def drop(n :Int) :Iterator[Y] = set(underlying.drop(n))
//		override def take(n :Int) :Iterator[Y] = set(underlying.take(n))
//		override def slice(from :Int, until :Int) :Iterator[Y] = set(underlying.slice(from, until))
//
//		protected def set(it :Iterator[X]) :Iterator[Y]
//	}

	final class ConstInfinite[+A](override val head :A) extends AbstractIterator[A] with  BufferedIterator[A] {
		override def hasNext :Boolean = true
		override def next() :A = head
		override def drop(n :Int) :Iterator[A] = this
		override def take(n :Int) :Iterator[A] = this
		override def slice(from :Int, until :Int) :Iterator[A] = this
		override def toString :String = "Iterator.infinite(" + head + "*)"
	}
	final class Const[+A](initSize :Int, override val head :A)
		extends KnownSizeVar[A](initSize) with BufferedIterator[A]
	{
		override def next() :A = {
			val size = knownSize
			if (size <= 0)
				throw new NoSuchElementException("Iterators.Const(" + head + ")")
			knownSize -= 1; head
		}
		override def toString :String = "Iterator.const<" + knownSize + ">(" + head + ")"
	}

	private[collections] abstract class AbstractFlatMap[+Y] extends AbstractIterator[Y] {
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


	private[Iterators] trait AbstractWithIndex[X, +Y] extends Iterator[Y] {
		protected def iter :Iterator[X]
		protected def dropped(n :Int, curr :Iterator[X]) :Unit

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
		override def knownSize :Int = underlying.knownSize
		override def hasNext :Boolean = underlying.hasNext
		override def next() :Y = { i += 1; f(underlying.next(), i) }

		protected override def iter :Iterator[X] = underlying
		protected override def dropped(n :Int, curr :Iterator[X]) :Unit = { i += n; underlying = curr }
		override def toString :String = underlying.toString + ".mapWithIndex(@" + i + ")"
	}

	final class FlatMapWithIndex[X, +Y](private[this] var source :Iterator[X], f :(X, Int) => IterableOnce[Y])
		extends AbstractFlatMap[Y] with AbstractWithIndex[X, Y]
	{
		private[this] var i = -1
		override def hasNext :Boolean = {
			var nonEmpty = super.hasNext
			while (!nonEmpty && source.hasNext) {
				i += 1
				val items = f(source.next(), i)
				val size =  items.knownSize
				nonEmpty = size != 0 && enqueue(items.iterator)
			}
			nonEmpty
		}

		protected override def iter :Iterator[X] = source
		protected override def dropped(n :Int, curr :Iterator[X]) :Unit = { i += n; source = curr }
		override def toString :String = source.toString + ".flatMapWithIndex(@" + i + ")"
	}

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
	}
	private val fallback :Any => Any = _ => fallback


	final class MapWhile[X, S, +Y](source :Iterator[X], private[this] var state :S,
	                               pred :S => Boolean, f :(S, X) => (S, Y))
		extends AbstractIterator[Y]
	{
		override def hasNext :Boolean = source.hasNext && pred(state)
		override def next() :Y = {
			val (a, res) = f(state, source.next())
			state = a
			res
		}
		override def toString :String = source.toString + ".mapWhile(" + state + ")"
	}
	final class FlatMapWhile[X, A, +Y](source :Iterator[X], private[this] var state :A,
	                                   pred :A => Boolean, f :(A, X) => (A, IterableOnce[Y]))
		extends AbstractFlatMap[Y]
	{
		override def hasNext :Boolean = {
			var nonEmpty = super.hasNext
			while (!nonEmpty && source.hasNext && pred(state)) {
				val (a, items) = f(state, source.next())
				state = a
				val size  = items.knownSize
				nonEmpty = size != 0 && enqueue(items.iterator)
			}
			nonEmpty
		}
	}

	final class MapSome[X, S, +Y](source :Iterator[X], private[this] var state :S, f :(S, X) => Option[(S, Y)])
		extends AbstractIterator[Y]
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
	final class FlatMapSome[X, S, +Y](source: Iterator[X], private[this] var state: S,
	                                  f: (S, X) => Option[(S, IterableOnce[Y])])
		extends AbstractFlatMap[Y]
	{
		private[this] var hd :Option[(S, IterableOnce[Y])] = _

		override def hasNext :Boolean = {
			var nonEmpty = super.hasNext
			while (!nonEmpty && hd == null && source.hasNext) {
				hd = f(state, source.next())
				hd match {
					case Some((a, items)) =>
						state = a
						val size = items.knownSize
						hd = null
						nonEmpty = size != 0 && enqueue(items.iterator)
					case _ =>
				}
			}
			nonEmpty
		}
	}


	final class FilterWith[E, S](underlying :Iterator[E], private[this] var state :S,
	                             f :(E, S) => (Boolean, S), keep :Boolean = true)
		extends AbstractBufferedIterator[E]
	{
		override def hasNext :Boolean = super.hasNext || {
			while (underlying.hasNext) {
				val peek = underlying.next()
				val (include, s) = f(peek, state)
				state = s
				if (include == keep) {
					push(peek)
					return true
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

	final class FilterWithIndex[E](private[this] var underlying :Iterator[E], f :(E, Int) => Boolean, keep :Boolean = true)
		extends AbstractBufferedIterator[E]
	{
		private[this] var i = -1

		override def hasNext :Boolean = super.hasNext || {
			while (underlying.hasNext) {
				val peek = underlying.next()
				i += 1
				if (f(peek, i) == keep) {
					push(peek)
					return true
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



	final class ZipEven[+A, +B](i1 :Iterator[A], i2 :Iterator[B]) extends AbstractIterator[(A, B)] {
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

		override def toString :String= "(" + i1 + " zipEven " + i2 + ")"
	}

	final class ZipEven3[+A, +B, +C](i1 :Iterator[A], i2 :Iterator[B], i3 :Iterator[C])
		extends AbstractIterator[(A, B, C)]
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

		override def toString :String= i1.toString + ".zipEven(" + i2 + ", " + i3 + ")"
	}


	final class Zip3[+A, +B, +C](i1 :Iterator[A], i2 :Iterator[B], i3 :Iterator[C])
		extends AbstractIterator[(A, B, C)]
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
		override def toString :String = "Iterator.zip3(" + i1 + ", " + i2 + ", " + i3 + ")"
	}

	final class ZipAll3[+A, +B, +C](
		private[this] var i1 :Iterator[A], pad1 :A,
		private[this] var i2 :Iterator[B], pad2 :B,
		private[this] var i3 :Iterator[C], pad3 :C
	)   extends AbstractIterator[(A, B, C)]
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

		override def toString :String =
			"Iterator.zipAll3(" + i1 + "++" + pad1 + "*, " + i2 + "++" + pad2 + "*, "  + i3 + "++" + pad3 + "*)"
	}



	final class Removed[+E](private[this] var underlying :Iterator[E], index :Int)
		extends AbstractBufferedIterator[E]
	{
		private[this] var i = 0 //number of consumed items from underlying, not the number o consumed items from this
		def offset :Int = i

		override def knownSize :Int = {
			val size = underlying.knownSize
			if (size < 0) -1
			else if (i > index) size + super.hasNext.toInt
			else if (i > index - size) size - 1
			else outOfBounds_!(index, i + size)
		}
		override def hasNext :Boolean = super.hasNext || {
			if (i == index) try {
				underlying.next()
				i += 1
			} catch {
				case e :NoSuchElementException =>
					throw new IndexOutOfBoundsException(index.toString + " out of " + index).initCause(e)
			}
			if (underlying.hasNext) {
				push(underlying.next())
				i += 1
				true
			} else {
				if (i < index)
					outOfBounds_!(index, i)
				false
			}
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0) {
				var toDrop = n
				if (super.hasNext) {        //essentially, we need to reduce n by 1
					toDrop -= 1
					pop()
				}
				if (i > index)               //don't increase i to avoid overflow
					underlying = underlying.drop(toDrop)
				else if (i < index - toDrop) {
					underlying = underlying.drop(toDrop)
					i += n
				} else {
					val dropAfterIndex = toDrop - index + i //overflow conscious
					underlying = underlying.drop(index - i)
					i = index
					if (hasNext)             //mainly to validate and throw an exception if index out of range
						drop(dropAfterIndex) //hasNext already has dropped the first (that is, index-th) element
				}
			}
			this
		}

		override def toString :String =
			if (super.hasNext)
				"Iterator@" + i + "(" + head + ",...)"
			else
				underlying.toString + "@" + i + ".removed(" + index + ")"
	}

	//an overflow conscious implementation
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
				push(underlying.next())
				i += 1
				true
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
		extends AbstractIterator[E]
	{
		private[this] var i = 0
		override def hasNext :Boolean =
			if (underlying.hasNext) true
			else if (index >= i) throw new IndexOutOfBoundsException(index)
			else false

		override def next() :E = {
			if (i == index) {
				i += 1
				underlying.next()
				elem
			} else try {
				val res = underlying.next()
				i += 1
				res
			} catch { //consider: is Iterator.next() guaranteed to throw a NoSuchElementException?
				case e :NoSuchElementException if index >= i =>
					throw new IndexOutOfBoundsException(index.toString + " out of " + i).initCause(e)
			}
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0) {
				underlying = underlying.drop(n)
				i += n
			}
			this
		}

		override def toString :String = underlying.toString + ".updated(@" + (index - i) + "=" + elem + ")"
	}

	final class UpdatedAll[+E](private[this] var underlying :Iterator[E], index :Int, private[this] var elems :Iterator[E])
		extends AbstractIterator[E]
	{
		if (index < 0)
			throw new IndexOutOfBoundsException(toString)

		private[this] var i = 0
		override def hasNext :Boolean =
			underlying.hasNext || {
				if (index >= i)
					throw new IndexOutOfBoundsException(toString)
				false
			}
		override def next() :E = {
			if (i >= index && elems.hasNext) {
				if (!underlying.hasNext)
					throw new IndexOutOfBoundsException(toString)
				i += 1
				underlying.next()
				elems.next()
			} else {
				i += 1
				underlying.next()
			}
		}
		override def drop(n :Int) :Iterator[E] = {
			if (n > 0) {
				underlying = underlying.drop(n)
				if (i > index - n)
					elems = elems.drop(n - (i - index))
				i += n
			}
			this
		}

		override def toString :String = underlying.toString + ".updatedAll(@" + (index - i) + "=" + elems + ")"
	}


	final class Inserted[+E](private[this] var underlying :Iterator[E], index :Int, elem :E)
		extends AbstractIterator[E]
	{
//		override protected def nextInserted() :E = elem
		private[this] var i = 0

		override def hasNext :Boolean =
			underlying.hasNext || index == i || {
				if (index > i)
					throw new IndexOutOfBoundsException(index.toString + " out of " + i)
				false
			}
		override def next() :E = {
			val res =
				if (i == index)
					elem
				else try
					underlying.next()
				catch {
					case e :NoSuchElementException if index > i =>
						throw new IndexOutOfBoundsException(index.toString + " out of " + i).initCause(e)
				}
			i += 1
			res
		}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				i += n
				underlying = underlying.drop(if (i <= index) n else n - 1)
				this
			}

		override def toString :String = underlying.toString + "@" + i + ".inserted(" + index + ", " + elem + ")"
	}
	final class InsertedAll[+E](private[this] var underlying :Iterator[E], index :Int, elems :Iterator[E])
		extends AbstractIterator[E]
	{
		private[this] var i = 0
		override def hasNext :Boolean =
			underlying.hasNext || index == i || {
				if (index > i)
					throw new IndexOutOfBoundsException(index.toString + " out of " + i)
				false
			}
		override def next() :E = {
			val res =
				if (i >= index && elems.hasNext)
					elems.next()
				else try
					underlying.next()
				catch {
					case e :NoSuchElementException if index >= i =>
						throw new IndexOutOfBoundsException(index.toString + " out of " + i).initCause(e)
				}
			i += 1
			res
		}
		override def drop(n :Int) :Iterator[E] =
			if (n <= 0)
				this
			else {
				i += n
				underlying = underlying.drop(if (i < index - n) n else n - 1)
				this
			}

		override def toString :String = {
			val elemsSize = elems.knownSize
			val elemsString = if (elemsSize >= 0) elems.className +  "<" + elemsSize + ">" else elems.className
			underlying.toString + "@" + i + ".insertedAll(" + index + ", " +  elemsString + ")"
		}
	}


	//consider: integrating the idea behind PassedArray and including the array directly.
	final class Concat[+E](first :Iterator[E], private[this] var iterators :PassedArray[Iterator[E]])
		extends AbstractFlatMap[E]
	{
		def this(iterators :PassedArray[Iterator[E]]) = this(iterators.head, iterators.tail)
		def this(first :Iterator[E], second :Iterator[E]) = this(first, PassedArray.single(second))

		enqueue(first)

		override def knownSize :Int = {
			var i = 0; val end = iterators.length
			var size = 0
			while (i < end && { val s = iterators(i).knownSize; size += s; s >= 0 })
				i += 1
			if (i == end) size
			else -1
		}

		def append[U >: E](next :Iterator[U]) :Concat[U] =
			if (!next.hasNext) this else new Concat(source, iterators :+ next)

		def prepend[U >: E](prev :Iterator[U]) :Concat[U] =
			if (!prev.hasNext) this
			else new Concat(prev, if (super.hasNext) source +: iterators else iterators)

		override def hasNext :Boolean = super.hasNext || {
			var i = 0; val iterCount = iterators.size
			while (i < iterCount && !enqueue(iterators(i)))
				i += 1
			iterators = iterators.drop(i)
			i < iterCount
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
	extends AbstractIterator[Y] with BufferedIterator[Y]
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

	/** Sets the `head` value of the iterator, and `hasNext` to `true`.
	  * This method is typically called by [[net.noresttherein.sugar.collections.AbstractBufferedIterator.hasNext hasNext]]
	  * before it returns `true`. Calling those methods has an additional effect in that `hasNext` implementation
	  * defined here will return `true`.
	  * @see [[net.noresttherein.sugar.collections.AbstractBufferedIterator.pop pop]]
	  */
	protected def push(head :Y@uncheckedVariance) :Unit = {
		hd = head
		knownNonEmpty = true
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
	  *                     true
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

	/** Delegates to `take` and `drop`, always 'dropping' before 'taking', in case a subclass decides to provide
	  * a more efficient `drop`.
	  */
	override def slice(from :Int, until :Int) :Iterator[Y] =
		if (until <= 0 | until <= from) Iterator.empty
		else if (from <= 0) take(until)
		else drop(from).take(until - from) //important to call drop first, as it is more likely to be overridden

	override def toString :String =
		this.localClassName + "(" + (if (knownNonEmpty) head + ", ...)" else "<not computed>)")
}
