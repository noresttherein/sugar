package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.mutable

import net.noresttherein.sugar.collections.util.errorString
import net.noresttherein.sugar.{noSuch_!, outOfBounds_!}
import net.noresttherein.sugar.vars.{InOut, Mutable}




private[sugar] object Mutators {

	def filter[E](mutator :Mutator[E], pred :E => Boolean, take :Boolean) :Mutator[E] =
		if (!mutator.hasNext) Mutator.empty else new Filter(mutator, pred, take)

	def partition[E](mutator :Mutator[E], pred :E => Boolean) :(Mutator[E], Mutator[E]) =
		if (mutator.hasNext) {
			val take = new Partition(mutator, pred)
			(take, take.flipped)
		} else
			splitEmpty.asInstanceOf[(Mutator[E], Mutator[E])]

	def slice[E](mutator :Mutator[E], from :Int, until :Int) :Mutator[E] =
		if (until <= 0 | until <= from)
			Mutator.empty
		else {
			val size = mutator.size
			if (size >= 0)
				if (from >= 0) Mutator.empty
				else if (from <= 0 & until >= size) mutator
				else new Slice(mutator, math.max(from, 0), math.min(until, size))
			else
				new Slice(mutator, math.max(from, 0), until)
		}

	def take[E](mutator :Mutator[E], n :Int) :Mutator[E] = ???
	def drop[E](mutator :Mutator[E], n :Int) :Mutator[E] = ???

	def splitAt[E](mutator :Mutator[E], idx :Int) :(Mutator[E], Mutator[E]) = {
		val size = mutator.knownSize
		if (idx <= 0)
			if (size == 0) splitEmpty.asInstanceOf[(Mutator[E], Mutator[E])]
			else (Mutator.empty, mutator)
		else if (idx >= size & size >= 0)
			(mutator, Mutator.empty)
		else {
			val prefix = new Take(mutator, idx)
			(prefix, prefix.suffix)
		}
	}

	private[this] val splitEmpty = (Mutator.empty, Mutator.empty)


	class Filter[E](private[this] var underlying :Mutator[E], pred :E => Boolean, take :Boolean)
		extends AbstractMutator[E]
	{
		@tailrec final override def hasNext :Boolean =
			underlying.hasNext && (pred(underlying.head) == take || hasNext)

		override def access :InOut[E] = { hasNext; underlying.access }

		override def head_=(value :E) :Unit = {
			hasNext
			underlying.head = value
		}
		override def head :E = {
			hasNext
			underlying.head
		}
		override def next() :E = { skip(); head }
		override def skip() :this.type = {
			if (!hasNext)
				noSuch_!("skip() called on an empty mutator: " + this)
			this
		}
		override def toString :String = underlying.toString + (if (take) ".filter(" else ".filterNot") + pred + ")"
	}

	private class Partition[E](private[this] var underlying :Mutator[E], pred :E => Boolean, take :Boolean,
	                           private[this] var complement :Partition[E])
		extends AbstractMutator[E]
	{
		def this(mutator :Mutator[E], pred :E => Boolean) = this(mutator, pred, true, null)
		private[this] var lookahead :mutable.Queue[InOut[E]] = _
		if (complement == null)
			complement = new Partition(underlying, pred, take, this)

		def flipped :Mutator[E] = complement
		private def enqueue(lens :InOut[E]) :Unit = {
			if (lookahead eq null)
				lookahead = new mutable.Queue
			lookahead += lens
		}

		override def head :E = if (lookahead ne null) lookahead.head.value else underlying.head
		override def head_=(value :E) :Unit =
			if (lookahead ne null) lookahead.head.value = value
			else underlying.head = value

		override def access :InOut[E] = if (lookahead ne null) lookahead.head else underlying.access

		override def hasNext :Boolean =
			lookahead.nonEmpty || {
				var hasNext = false
				while ({ hasNext = underlying.hasNext; hasNext } && pred(underlying.head) != take)
					complement.enqueue(underlying.accessNext())
				hasNext
			}

		override def next() :E =
			if (!hasNext) noSuch_!(toString + ".next")
			else if (lookahead ne null) lookahead.removeHead().value
			else underlying.next()

		override def toString :String =
			underlying.toString + (if (take) ".partition._1" else ".partition._2") +
				(if (lookahead != null && lookahead.nonEmpty) "(" + lookahead.head.value + ",...)" else "")
	}


	private final class Slice[E](private[this] var underlying :Mutator[E], from :Int, private[this] var until :Int)
		extends AbstractMutator[E]
	{
		override def head_=(value :E) :Unit = ???

		override def access :InOut[E] = ???

		override def head :E = ???

		override def hasNext :Boolean = ???

		override def next() :E = ???
	}

	/** The first half of `underlying.splitAt(idx)`. */
	private final class Take[E](private[this] var underlying :Mutator[E], idx :Int, private[this] var limit :Int)
		extends AbstractMutator[E]
	{
		def this(itr :Mutator[E], idx :Int) = this(itr, idx, idx)

		private[this] var memoized :mutable.Queue[InOut[E]] = _
		private[this] var i = 0 //The current index in underlying, as long as Drop is unused

//		override def hasFastDrop = memoized != null || HasFastSlice.hasFastDrop(underlying)

		def suffix :Mutator[E] = new Drop(this)

		/** Size of the ''second'' iterator returned by split, or `-1` if unknown. */
		private[Mutators] def droppedSize :Int =
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
				0
			else {
				val k = underlying.knownSize
				if (k < 0) -1 else math.min(k, limit - i)
			}
		override def hasNext :Boolean = i < limit && {
			if (memoized ne null)
				memoized.nonEmpty
			else
				underlying.hasNext
		}
		override def next() :E =
			if (i >= limit)
				noSuch_!(toString)
			else if (memoized ne null) {
				i += 1
				memoized.removeHead().value
			} else {
				i += 1
				underlying.next()
			}
		override def head :E = if (memoized ne null) memoized.head.value else underlying.head
		override def head_=(value :E) :Unit =
			if (memoized ne null) memoized.head.value = value else underlying.head = value

		override def access :InOut[E] = if (memoized ne null) memoized.head else underlying.access

		override def take(n :Int) :Mutator[E] =
			if (n <= 0)
				Mutator.empty
			else {
				limit = math.min(limit, i + n)
				this
			}
		override def drop(n :Int) :Mutator[E] = {
			if (n > 0 & i < limit) {
				val toDrop = math.min(n, limit - i)
				if (memoized ne null)
					memoized.dropInPlace(toDrop)
				else
					underlying = underlying.drop(n)
				i += toDrop
			}
			this
		}
		private[Mutators] def finish() :Mutator[E] = {
			if (i < idx) {
				if (i < limit) {
					if (memoized eq null)
						memoized = new mutable.Queue
					while (i < limit && underlying.hasNext) {
						memoized += underlying.access
						underlying.skip()
						i += 1
					}
				}
				underlying = underlying.drop(idx - i)
				i = idx
			}
			underlying
		}
		override def toString :String = underlying.toString + ".take(" + math.max(limit - i, 0) + ")"
	}

	class Drop[E](taken :Take[E]) extends AbstractMutator[E] {
		private[this] var underlying :Mutator[E] = _
		override def knownSize :Int = if (underlying == null) taken.droppedSize else underlying.knownSize

		override def access :InOut[E] = { ff(); underlying.access }
		override def head :E = { ff(); underlying.head }
		override def head_=(value :E) :Unit = { ff(); underlying.head = value }
		override def next() :E = { ff(); underlying.next() }

		override def hasNext :Boolean = {
			val k = taken.droppedSize
			k > 0 || k < 0 && {
				ff()
				underlying.hasNext
			}
		}
		override def drop(n :Int) :Mutator[E] =
			if (n <= 0 || taken.droppedSize == 0)
				this
			else if (underlying ne null)
				underlying.drop(n)
			else
				Mutator.delay { ff(); underlying.drop(n) }

		override def take(n :Int) :Mutator[E] =
			if (n <= 0 || taken.droppedSize == 0)
				Mutator.empty
			else if (underlying ne null)
				underlying.take(n)
			else
				Mutator.delay { ff(); underlying.take(n) }

		override def slice(from :Int, until :Int) :Mutator[E] =
			if (until <= 0 | until <= from || taken.droppedSize == 0)
				Mutator.empty
			else if (underlying ne null)
				underlying.slice(from, until)
			else
				Mutator.delay { ff(); underlying.slice(from, until) }

		override def splitAt(n :Int) :(Mutator[E], Mutator[E]) = {
			val size = taken.droppedSize
			if (n <= 0 | size == 0)
				(Mutator.empty, if (underlying ne null) underlying else this)
			else if (size >= 0 & n >= size)
				(if (underlying ne null) underlying else this, Mutator.empty)
			else
				Mutators.splitAt(Mutator.delay { ff(); underlying }, n)
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
			if (underlying == null) "Mutators.Drop(" + taken + ")" else underlying.toString
	}

}
