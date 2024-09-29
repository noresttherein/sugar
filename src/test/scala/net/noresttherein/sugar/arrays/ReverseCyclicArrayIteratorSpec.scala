package net.noresttherein.sugar.arrays

import scala.collection.BufferedIterator




trait ReverseCyclicIndexedIteratorProps[S[_], I[X] <: BufferedIterator[X]] extends IndexedIteratorProps[S, I] {
	protected def mod(idx :Int, len :Int) :Int =
		if (len == 0) 0
		else if (idx < 0) (len + idx % len) % len
		else idx % len

	protected override def expectSlice[X](source :S[X], from :Int, until :Int) :Seq[X] = {
		val len = lengthOf(source)
		val from0 = mod(from, len)
		val until0 = mod(until, len)
		if (from0 < until0)
			slice(source, from0, until0).reverse
		else if (until0 < from0)
			(iterator(source).drop(from0) ++ iterator(source).take(until0)).toSeq.reverse
		else if (from == until)
			Seq.empty
		else  //from0 == until0
			(iterator(source).drop(from0) ++ iterator(source).take(until0)).toSeq.reverse
	}

	protected override def expectApply[X](source :S[X], first :Int, length :Int) :Seq[X] = {
		val len = lengthOf(source)
		val until = mod(mod(first, len) + 1, len)
		val iter =
			if (until >= length)
				slice(source, 0, until).reverseIterator.take(length)
			else
				slice(source, 0, until).reverseIterator ++
					seq(source).reverseIterator.take(math.min(len, length) - until)
		iter.toSeq
	}

	protected override def expectFrom[X](source :S[X], first :Int) :Seq[X] =
		expectApply(source, first, lengthOf(source))
}


object ReverseCyclicArrayIteratorSpec
	extends ArrayIteratorProps[BufferedIterator]("ReverseCyclicArrayIterator", ReverseCyclicArrayIterator)
	   with ReverseCyclicIndexedIteratorProps[Array, BufferedIterator]
