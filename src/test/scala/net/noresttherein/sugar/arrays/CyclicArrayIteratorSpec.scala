package net.noresttherein.sugar.arrays

import scala.collection.BufferedIterator



trait CyclicIndexedIteratorProps[S[_], I[X] <: BufferedIterator[X]] extends IndexedIteratorProps[S, I] {
	protected def mod(idx :Int, len :Int) :Int =
		if (len == 0) 0
		else if (idx < 0) (len + idx % len) % len
		else idx % len

	protected override def expectSlice[X](source :S[X], from :Int, until :Int) :Seq[X] = {
		val len    = lengthOf(source)
		val from0  = mod(from, len)
		val until0 = mod(until, len)
		val iter =
			if (from0 < until0)
				iterator(source).slice(from0, until0)
			else if (until0 < from0)
				iterator(source).drop(from0) ++ iterator(source).take(until0)
			else if (from == until)
				Iterator.empty
			else
				iterator(source).drop(from0) ++ iterator(source).take(from0)
		iter.toSeq
	}

	protected override def expectApply[X](source :S[X], first :Int, length :Int) :Seq[X] = {
		val len = lengthOf(source)
		val size = math.max(0, math.min(len, length))
		val rem = mod(first, len)
		val suffix = iterator(source).drop(rem).take(length)
		(if (len - rem >= size) suffix else suffix ++ iterator(source).take(size - (len - rem))).toSeq
	}

	protected override def expectFrom[X](source :S[X], first :Int) :Seq[X] = expectApply(source, first, Int.MaxValue)
}




object CyclicArrayIteratorSpec
	extends ArrayIteratorProps[BufferedIterator]("CyclicArrayIterator", CyclicArrayIterator)
	   with CyclicIndexedIteratorProps[Array, BufferedIterator]
