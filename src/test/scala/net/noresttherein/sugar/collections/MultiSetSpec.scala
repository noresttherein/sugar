package net.noresttherein.sugar.collections
import scala.collection.IterableFactory




/**
  * @author Marcin Mościcki
  */
object MultiSetSpec extends IterableProps[MultiSet, Iterable]("MultiSet") {
	//todo: test MultiSet specific methods

	override def referenceFactory :IterableFactory[Iterable] = UnorderedItems
	override def checkedFactory :IterableFactory[MultiSet] = MultiSet

	override def knowsSize = true
	override def hasOrder = false
}