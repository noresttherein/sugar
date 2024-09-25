package net.noresttherein.sugar.collections

import scala.collection.mutable.Growable
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.Arbitrary


object SpillArrayBufferSpec {

	private class SpillBufferProps(cap :Int)
		extends IterableProps[SpillArrayBuffer, SpillBuffer](SpillArrayBuffer.cappedFactory(cap), ???)
		   with SeqProps[SpillArrayBuffer, SpillBuffer, Dummy]
//		   with SugaredIterableProps[CappedArraySeq, collection.Seq, Dummy]
	{
		override def knowsSize = true

		protected implicit override val anyEvidence :E[Any] = new Dummy

		//Made public for use in CappedArraySeqSpec
		override def props[T, F, M, FM](expect :SpillBuffer[T], result :SpillArrayBuffer[T])
		                               (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
		                                filt :IterableProps.Filter[T], fldA :IterableProps.FoldSide[F, T], evf :E[F],
		                                fld :IterableProps.Fold[T], mp :IterableProps.Map[T, M], evm :E[M],
		                                fmap :IterableProps.FlatMap[T, FM], evfm :E[FM]) =
			super.props(expect, result)
	}
}
