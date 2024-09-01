package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, IterableOps, View}
import scala.collection.immutable.{LinearSeq, SeqOps}

import net.noresttherein.sugar.collections.HasFastSlice.hasFastDrop
import net.noresttherein.sugar.collections.IndexedIterable.applyPreferred
import net.noresttherein.sugar.collections.util.{HasFastPrepend, HasFastReverse, errorString}
import net.noresttherein.sugar.collections.extensions.IterableOnceExtension
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.reflect.extensions.classNameMethods




private object Defaults {
	/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
	  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
	  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
	  * which may make it slightly more efficient than `patch`.
	  */
	/* Consider: should index be permissive in regard to the valid range? in updated it's not; in patch it is.
	 * I don't like the semantics of patch: permissive indices should result in no effect for the indices
	 * out of range, not simply truncating them. We can't however just validate before calling patch if we don't
	 * know the sizes, but we would like to use patch in case it has a more efficient implementation.
	 */
	def updatedAll[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		def outOfBounds(msg :String = "") =
			outOfBounds_!(
				errorString(self) + ".updatedAll(" + index + ", " + errorString(elems) + ")" +
					(if (msg.nonEmpty) ": " + msg else msg)
			)
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case _ if index < 0 || thisSize >= 0 & index > thisSize - math.max(thatSize, 0) =>
				outOfBounds()
			case _ :View[_] =>
				self.iterableFactory from Views.updatedAll(self.iterator, index, elems)
			case _ if thatSize == 0 & thisSize >= 0 & index <= thisSize =>
				//Default patch implementation doesn't check if the operation results in no changes.
				self.iterableFactory from self
			case seq :collection.SeqOps[E, CC, C] if thisSize >= 0 & thatSize >= 0 =>
				seq.patch(index, elems, thatSize)
			case _ if !self.knownStrict        =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
			case _ if elems.toBasicOps.isEmpty =>
				//Traversing the whole list to compute its length only to throw an exception is wasteful.
				if (self.sizeIs < index)
					outOfBounds_!(index, self.size)
				self.iterableFactory from self
			//Not collection.LinearSeq because we want to reuse the tail.
			case seq :LinearSeq[E] @unchecked  =>
				updatedAll(seq, index, elems, true, self.iterableFactory)
			case HasFastSlice(items)           =>
				updatedAll(items, index, elems, true, self.iterableFactory)
			case _                             =>
				self.iterableFactory from Iterators.updatedAll(self.iterator, index, elems)
		}
	}

	private def updatedAll[E, CC[_], C](seq :LinearSeq[E], index :Int, elems :IterableOnce[E],
	                                    validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		//We hope for fast tail, that hd +: tail reuses tail, and that iterableFactory from seq eq seq.
		val thisSize = seq.knownSize
		val thatSize = elems.knownSize
		var i = 0
		var initReversed :List[E] = Nil
		var tail :collection.LinearSeq[E] = seq
		while (i < index && tail.nonEmpty) {       //Move the seq prefix from before the patch aside, reversing it.
			initReversed = tail.head::initReversed
			tail = tail.tail
			i += 1
		}
		if (i < index)
			if (validate)
				outOfBounds_!(
					seq.className + "|" + (if (thisSize >= 0) thisSize.toString else index.toString + "+")
						+ "|.updatedAll(" + index + ", " + errorString(elems) + ")"
				)
			else
				factory from seq
		else {
			elems match {
				case list :collection.LinearSeq[E] =>
					var patch = list
					i = 0
					while (i < -index) {
						patch = patch.tail
						i += 1
					}
					while (patch.nonEmpty && tail.nonEmpty) { //Reverse patch, prepending it to our reversed prefix.
						initReversed = patch.head::initReversed
						patch = patch.tail
						tail = tail.tail
						i += 1
					}
					if (validate && tail.isEmpty && patch.nonEmpty)
						outOfBounds_!(
							errorString(seq) + ".updatedAll(" + index + ", " + elems.className + "|" +
								(if (thatSize >= 0) thatSize else i.toString + "+") + "|): patch too large"
						)
				case IndexedIterable(seq) => //matches only collections of known size
					val dstIdx = math.max(-index, 0)
					i = dstIdx //Ignore the first dstIdx elements, as they fall before index 0 in self.
					while (i < thatSize & tail.nonEmpty) { //Determine the length of the patch we should use.
						i   += 1
						tail = tail.tail
					}
					if (validate & i < thatSize && tail.isEmpty)
						outOfBounds_!(
							errorString(seq) + ".updatedAll(" + index + ", " + errorString(elems) + ": patch too large"
						)
					if (!applyPreferred(seq)) {
						var itr = seq.reverseIterator
						if (hasFastDrop(itr)) {
							itr = itr.drop(thatSize - i)
							while (i > dstIdx) {
								i   -= 1
								tail = itr.next() +: tail
							}
						}
					}
					//The loop will be entered only if we haven't already prepended patch in the preceding if block.
					while (i > dstIdx) {
						i -= 1
						tail = seq(i) +: tail
					}
				case _ =>
					val itr = HasFastSlice.drop(elems.iterator, math.max(0, -index))
					while (itr.hasNext && tail.nonEmpty) {
						initReversed = itr.next()::initReversed
						tail = tail.tail
						i += 1
					}
					if (validate && tail.isEmpty && itr.hasNext)
						outOfBounds_!(
							seq.className + "|" + i + "|.updatedAll(" + index + ", " + errorString(elems) +
								"|): patch too large"
						)
			}
			factory from util.prependReverse(initReversed, tail)
		}
	}

	//Called when self has fast slicing
	private def updatedAll[E, CC[_], C](self :collection.IterableOps[E, IterableOnce, IterableOnce[E]], index :Int,
	                                    elems :IterableOnce[E], validate :Boolean, factory :IterableFactory[CC]) :CC[E] =
	{
		def outOfBounds(elemsSize :Int) =
			outOfBounds_!(
				errorString(self) + ".updatedAll(" + index + ", " + util.className(elems) + "|" + elemsSize + "|)"
			)
		def assertExhausted(thisSize :Int, thatItr :Iterator[E]) :Unit =
			if (validate && thatItr.hasNext)
				outOfBounds_!(
					errorString(self) + ".updatedAll(" + index + ", " + errorString(elems) +
						" (size >= " + (thisSize - math.max(0, index)) + ")"
				)

		//If possible, try to add collections, rather than iterators, as there is a chance they'll reuse contents.
		val thisSize = self.knownSize
		val thatSize = elems.knownSize
		val res = factory.newBuilder[E]
		res sizeHint thisSize
		res ++= self.take(index)
		if (thisSize < 0) {
			//Default Iterator.drop creates a proxy even if n <= 0, so lets check it ourselves to avoid it.
			val thatItr = HasFastSlice.drop(elems.iterator, -index)
			val thisItr = HasFastSlice.drop(self.iterator, index)
			var i = math.max(0, index)
			while (thisItr.hasNext && thatItr.hasNext) {
				res += thatItr.next()
				thisItr.next()
				i += 1
			}
			assertExhausted(i, thatItr)
			res ++= thisItr
		} else if (thatSize < 0) {
			var i       = math.max(index, 0)
			val thatItr = HasFastSlice.drop(elems.iterator, -index)
			while (i < thisSize && thatItr.hasNext) {
				res += thatItr.next()
				i += 1
			}
			assertExhausted(i, thatItr)
			if (i < thisSize)
				res ++= self.drop(i)
		} else { //thisSize >= 0 && thatSize >= 0
			if (index <= thisSize - thatSize)
				res ++= HasFastSlice.drop(elems, -index) //Smart enough to do nothing if index >= 0
			else if (validate)
				outOfBounds(thatSize)
			else
				res ++= HasFastSlice.slice(elems, -index, thisSize - index)
			if (index < thisSize - thatSize)
				res ++= self.drop(index + thatSize)
		}
		res.result()
	}


	/** For indices in range, functionally equivalent to [[collection.SeqOps.patch patch]]`(index, elems, elems.size)`.
	  * It does ''not'' however use `size` method and may be implemented in a different manner, and the index
	  * must be in `0..this.length - elems.length` range, or an [[IndexOutOfBoundsException]] is thrown,
	  * which may make it slightly more efficient than `patch`.
	  */
	/* Consider: should index be permissive in regard to the valid range? in updated it's not; in patch it is.
	 * I don't like the semantics of patch: permissive indices should result in no effect for the indices
	 * out of range, not simply truncating them. We can't however just validate before calling patch if we don't
	 * know the sizes, but we would like to use patch in case it has a more efficient implementation.
	 */
	def overwritten[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] = {
		val thatSize = elems.knownSize
		val thisSize = self.knownSize
		self match {
			case _ if thatSize == 0 | thisSize == 0 || index <= 0 && thatSize >= 0 && index + thatSize <= 0
				|| thisSize >= 0 && thisSize <= index || index == Int.MinValue || index == Int.MaxValue
			=>
				self.iterableFactory from self
			case _ :View[_]          =>
				self.iterableFactory from Views.overwritten(self, index, elems)
			case seq :collection.SeqOps[E, CC, C] if thatSize >= 0 & thisSize >= 0 =>
				val srcIdx   = math.max(0, -index)
				val replaced = math.min(thisSize - math.max(0, index), thatSize - srcIdx)
				val that = HasFastSlice.slice(elems, srcIdx, srcIdx + replaced)
				seq.patch(index, that, replaced)
			case _ if !self.knownStrict =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
			case _ if elems.toBasicOps.isEmpty =>
				self.iterableFactory from self
			case seq :collection.LinearSeq[E] @unchecked =>
				updatedAll(seq, index, elems, false, self.iterableFactory)
			case HasFastSlice(items) =>
				updatedAll(items, index, elems, false, self.iterableFactory)
			case _ =>
				self.iterableFactory from Iterators.overwritten(self.iterator, index, elems)
		}
	}


	/** Inserts a new element to this sequence at the specified position, pushing all elements at `index`
	  * and beyond by one position. Equivalent to
	  * [[collection.SeqOps.patch patch]]`(index, Seq(elem), 1)`.
	  */ //todo: permissive indexing
	//Consider: use patch, in case it is overridden like in a Finger tree:
	// negative indices are treated as zero, while indices greater than the length
	// of this sequence result in appending the element to the end of the sequence.
	def inserted[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elem :E) :CC[E] = {
		val size = self.knownSize
		if (index < 0 | size >= 0 & index > size)
			outOfBounds_!(errorString(self) + ".inserted(" + index + ", _)")
		self match {
			case seq :collection.SeqOps[E, CC, C] if index == 0                => seq.prepended(elem)
			case seq :collection.SeqOps[E, CC, C] if size >= 0 & index == size => seq.appended(elem)
			case _ :View[_] => self.iterableFactory from Views.inserted(self, index, elem)
			case _ => self.iterableFactory from Iterators.inserted(self.iterator, index, elem)
		}
	}

	/** Equivalent to [[collection.SeqOps.patch patch]]`(index, elems, 0)`. */
	def insertedAll[E, CC[_], C](self :IterableOps[E, CC, C], index :Int, elems :IterableOnce[E]) :CC[E] =
		self match {
			case seq :collection.SeqOps[E, CC, C] if self.knownSize >= 0 =>
				val size = self.knownSize
				if (index < 0 || index > size)
					outOfBounds_!(errorString(self) + ".insertedAll(" + index + ", " + errorString(elems) + ")")
				seq.patch(index, elems, 0)
			case _ :View[_] => self.iterableFactory from Views.insertedAll(self, index, elems)
			case _ => //Can't use patch because insertedAll validates the index.
				self.iterableFactory from Iterators.insertedAll(self.iterator, index, elems)
		}


	def reversePrependedAll[E, CC[_], C](self :collection.SeqOps[E, CC, C], prefix :IterableOnce[E]) :CC[E] = {
		def reverseWithBuilder() = {
			val itr = Iterators.reverse(prefix)
			val res = self.iterableFactory.newBuilder[E]
			val thisSize = self.knownSize
			val thatSize = prefix.knownSize
			if (thisSize >= 0)
				if (thatSize >= 0)
					res.sizeHint(thisSize + thatSize)
				else
					res.sizeHint(itr, thisSize) //itr may iterate over an array with reversed contents of prefix
			res ++= itr ++= self
			res.result()
		}
		(prefix, self) match {
			case (list1 :List[E], list2 :List[E])    => self.iterableFactory from (list1 reverse_::: list2)
			case (HasFastReverse(reversed), _)       => self prependedAll reversed
			case (_, _ :SeqOps[E, CC, C] @unchecked) =>
				var res = self.iterableFactory from self //We assume it's a no-op if self is immutable.
				res match {
					case HasFastPrepend(ops) =>
						val itr = prefix.iterator
						while (itr.hasNext)
							res = ops.prepended(res)(itr.next())
						res
					case _ => reverseWithBuilder()
				}
			case _ => reverseWithBuilder()
		}
	}

}
