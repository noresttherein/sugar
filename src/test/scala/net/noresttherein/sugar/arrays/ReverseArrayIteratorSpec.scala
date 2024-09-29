package net.noresttherein.sugar.arrays

import scala.collection.BufferedIterator




trait ReverseIndexedIteratorProps[S[_], I[X] <: BufferedIterator[X]] extends IndexedIteratorProps[S, I] {
	protected override def expectSlice[X](source :S[X], from :Int, until :Int) :Seq[X] =
		slice(source, from, until).reverse

	protected override def expectApply[X](source :S[X], first :Int, length :Int) :Seq[X] =
		slice(source, 0, math.min(first, lengthOf(source) - 1) + 1).reverseIterator.take(length).toSeq

	protected override def expectFrom[X](source :S[X], first :Int) :Seq[X] =
		slice(source, 0, math.min(Int.MaxValue - 1, first) + 1).reverseIterator.toSeq
}


//todo: move it to the file with ArrayIteratorSpec
object ReverseArrayIteratorSpec
	extends ArrayIteratorProps[BufferedIterator]("ReverseArrayIterator", ReverseArrayIterator)
	   with ReverseIndexedIteratorProps[Array, BufferedIterator]
