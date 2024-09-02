package net.noresttherein.sugar.collections

import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.Arbitrary


object CappedArrayBufferSpec {

	private object SeqProps
		extends UntaggedSeqProps[CappedArrayBuffer](CappedArrayBuffer)
//		   with SugaredIterableProps[CappedArraySeq, collection.Seq, Dummy]
	{
		override def knowsSize = true

		//Made public for use in CappedArraySeqSpec
		override def props[T, F, M, FM](expect :collection.Seq[T], result :CappedArrayBuffer[T])
		                               (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
		                                filt :IterableProps.Filter[T], fldA :IterableProps.FoldSide[F, T], evf :E[F],
		                                fld :IterableProps.Fold[T], mp :IterableProps.Map[T, M], evm :E[M],
		                                fmap :IterableProps.FlatMap[T, FM], evfm :E[FM]) =
			super.props(expect, result)
	}
}
