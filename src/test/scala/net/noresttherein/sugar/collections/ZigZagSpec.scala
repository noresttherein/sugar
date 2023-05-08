package net.noresttherein.sugar.collections

import scala.collection.IterableFactory




object ZigZagSpec extends SeqProps[ZigZag]("ZigZag") {
//	override def referenceFactory = List
	override def checkedFactory :IterableFactory[ZigZag] = ZigZag
	override def knowsSize = true
}
