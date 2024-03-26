package net.noresttherein.sugar.collections

import net.noresttherein.sugar.collections.IterableProps.Dummy




object SListSpec
	extends UntaggedSeqProps[SList](SList)
	   with SugaredIterableProps[SList, collection.Seq, Dummy]
	   with PatchingProps[SList, collection.Seq, Dummy]
{
	override def knowsSize = true
}
