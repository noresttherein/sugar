package net.noresttherein.sugar.collections
import scala.collection.IterableFactory




/**
  * @author Marcin Mościcki
  */
object MultiSetSpec extends IterableProps[MultiSet, Iterable]("MultiSet")(MultiSet, UnorderedItems) {
	//todo: test MultiSet specific methods

	override def knowsSize = true
	override def hasOrder = false
}