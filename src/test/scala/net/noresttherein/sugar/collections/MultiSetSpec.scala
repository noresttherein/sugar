package net.noresttherein.sugar.collections
import scala.collection.IterableFactory

import net.noresttherein.sugar.collections.IterableProps.Dummy




/**
  * @author Marcin Mościcki
  */
object MultiSetSpec
	extends IterableProps[MultiSet, Iterable](MultiSet, UnorderedItems)
	   with SugaredIterableProps[MultiSet, Iterable, Dummy]
{
	//todo: test MultiSet specific methods

	override def knowsSize = true
	override def hasOrder = false
}