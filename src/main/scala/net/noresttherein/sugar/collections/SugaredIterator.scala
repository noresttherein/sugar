package net.noresttherein.sugar.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterator, BufferedIterator}

import net.noresttherein.sugar.arrays.ArrayCompanionExtension
import net.noresttherein.sugar.collections.Constants.MaxArraySize
import net.noresttherein.sugar.exceptions.noSuch_!
import net.noresttherein.sugar.reflect.extensions.classNameMethods




trait SugaredIterator[+E] extends Iterator[E] {
	def hasFastDrop :Boolean = knownSize == 0
	def strictDrop(n :Int) :Iterator[E] = {
		var left = n
		while (left > 0 && hasNext) {
			next()
			left -= 1
		}
		this
	}
	def strictSlice(from :Int, until :Int) :Iterator[E] =
		if ({ val size = knownSize; size >= 0 & until >= size })
			strictDrop(from)
		else
			strictDrop(from).take(until - math.max(0, from))

	override def drop(n :Int) :Iterator[E] =
		if (n <= 0) this
		else if ({ val size = knownSize; size >= 0 & n >= size }) Iterator.empty
		else super.drop(n)

	override def take(n :Int) :Iterator[E] =
		if (n <= 0) Iterator.empty
		else if ({ val size = knownSize; size >= 0 & n >= size }) this
		else super.take(n)

	override def slice(from :Int, until :Int) :Iterator[E] =
		if (until <= 0 | until <= from) Iterator.empty
		else if (from <= 0) take(until)
		else if ({ val size = knownSize; size >= 0 & until >= size }) drop(from)
		else super.slice(from, until)

	override def splitAt(n :Int) :(Iterator[E], Iterator[E]) = Iterators.splitAt(this, n)
}


/** Overrides `slice` in terms of `drop` and `take`, starting with `drop`,
  * hoping the subclass has a better implementation.
  */
private[sugar] trait IteratorWithDrop[+E] extends SugaredIterator[E] {
	//Todo: this will often result in double wrapping. Lets instead have a base class which handles take internally,
	// so it always returns this. Then this method starts to have more sense.
	override def slice(from :Int, until :Int) :Iterator[E] =
		if (until <= 0 | until <= from) Iterator.empty[E]
		else if (from <= 0) take(until)
		else drop(from).take(until - from)

	override def strictSlice(from :Int, until :Int) :Iterator[E] =
		if (until <= 0 | until <= from) Iterator.empty[E]
		else if ({ val size = knownSize; size >= 0 & until >= size }) strictDrop(from)
		else if (from <= 0) take(until)
		else strictDrop(from).take(until - from)
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
  *
  * The advantage of this scheme over always precomputing the `n+1`-th element immediately before
  * returning the `n`-th one from `next()`, is that it is lazy, and thus will never evaluate more elements
  * than actually requested, which is important for implementations which adapt other iterators of unknown origin.
  */
abstract class AbstractBufferedIterator[+Y]
	extends AbstractIterator[Y] with BufferedIterator[Y] with IteratorWithDrop[Y]
{
	private[this] var knownNonEmpty = false
	private[this] var hd :Y         = _

	/** Delegates to [[net.noresttherein.sugar.collections.AbstractBufferedIterator.hasNext hasNext]] to ensure
	  * that the next item is stored in the iterator, end either returns it, or throws a [[NoSuchElementException]],
	  * depending on the returned value.
	  */
	override def head :Y =
		if (knownNonEmpty || hasNext) hd
		else noSuch_!("empty iterator")

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
			noSuch_!("empty iterator")
		knownNonEmpty = false
		hd
	}

	override def toString :String =
		this.localClassName + "(" + (if (knownNonEmpty) head.toString + ",...)" else "<not computed>)")
}






/** A proxy iterator maintaining a counter of returned (or skipped) elements. */
class CountingIterator[+E](private[this] var underlying :Iterator[E], private[this] var counter :Int = 0)
	extends IteratorWithDrop[E]
{
	def total :Int = counter
	override def knownSize   :Int = underlying.knownSize
	override def hasFastDrop :Boolean = HasFastSlice(underlying)

	override def hasNext :Boolean = underlying.hasNext
	override def next() :E = { val res = underlying.next(); counter += 1; res }

	override def strictDrop(n :Int) :CountingIterator[E] = {
		if (n > 0) {
			val size = underlying.knownSize
			if (size >= 0) {
				counter += math.min(size, n)
				underlying = underlying.drop(n)
				underlying.hasNext //Enforce strictness.
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
	override def drop(n :Int) :CountingIterator[E] =
		if (n <= 0)
			this
		else {
			val size = underlying.knownSize
			if (size >= 0) {
				counter += math.min(size, n)
				underlying = underlying.drop(n)
				this
			} else
				new CountingIterator(underlying, total) {
					private[this] var dropped = false
					override def total = { drop(); super.total }
					override def hasNext = { drop(); super.hasNext }
					override def next() = { drop(); super.next() }
					override def take(n :Int) = { drop(); super.take(n) }
					override def strictDrop(n :Int) = { drop(); super.strictDrop(n) }
					override def drop(n :Int) = { drop(); super.drop(n) }
					private def drop() :Unit =
						if (!dropped) {
							super.strictDrop(n)
							dropped = true
						}
					override def slice(from :Int, until :Int) = { drop(); super.slice(from, until) }
					override def splitAt(n :Int) = { drop(); super.splitAt(n) }
					override def copyToArray[U >: E](xs :Array[U], start :Int, len :Int) :Int = {
						drop(); super.copyToArray(xs, start, len)
					}
					override def toString = { drop(); super.toString }
				}
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

	//Consider: should the second iterators start counting again from zero?
	override def splitAt(n :Int) :(CountingIterator[E], CountingIterator[E]) =
		if (n <= 0)
			(new CountingIterator(Iterator.empty, total), this)
		else {
			val size = underlying.knownSize
			if (size >= 0 & n >= size)
				(this, new CountingIterator(Iterator.empty, total + size))
			else {
				val (prefix, suffix) = Iterators.splitAt(underlying, n)
				val countingPrefix   = new CountingIterator(prefix, total)
				if (size >= 0)
					(countingPrefix, new CountingIterator(suffix, total + n))
				else {
					val expectedSplit = total + n
					val countingSuffix = new CountingIterator(suffix, expectedSplit) {
						override def total = {
							hasNext //fast forward prefix, if necessary
							super.total - (countingPrefix.total - expectedSplit)
						}
					}
					(countingPrefix, countingSuffix)
				}
			}
		}

	override def copyToArray[B >: E](xs :Array[B], start :Int, len :Int) :Int = {
		val copied = underlying.copyToArray(xs, start, len)
		counter += copied
		copied
	}

	override def toString :String = "CountingIterator(#" + counter + ": " + underlying + ")"
}






/* //Currently unused; could be an argument to IteratorExtension.:++ or some such.
abstract class IteratorContinuation[+E] extends Iterator[E] {
	private[this] lazy val instance = apply()
	def apply() :Iterator[E]

	override def knownSize :Int = instance.knownSize
	override def hasNext :Boolean = instance.hasNext
	override def next() :E = instance.next()
}
*/

/* //todo: GrowingIterator
class GrowingIterator[E] private (private[this] var iterators :Array[Iterator[E]],
                                   private[this] var start :Int, private[this] var end :Int)
	extends Iterators.AbstractFlatMap[E]
{
	def +=(elem :E) :this.type = this ++= Iterator.single(elem)

	def ++=(next :Iterator[E]) :this.type = {
		if (end == iterators.length) {
			iterators = Array.copyOfRange(iterators, start, end, math.min(MaxArraySize >> 1, iterators.length) << 1)
			end -= start
			start = 0
		}
		iterators(end) = next
		end += 1
		this
	}
}
*/