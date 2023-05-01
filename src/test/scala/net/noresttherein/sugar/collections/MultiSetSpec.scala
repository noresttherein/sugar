package net.noresttherein.sugar.collections

import scala.collection.IterableFactory
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.IterableProps.Filter
import net.noresttherein.sugar.extensions.{classNameMethods, iterableExtension}
import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.forAll


/**
  * @author Marcin Mościcki
  */
object MultiSetSpec extends IterableProps[MultiSet, Iterable]("MultiSet") {
	//todo: test MultiSet specific methods

	override def referenceFactory = UnorderedItems
	override def checkedFactory   = MultiSet

	override def knowsSize = true
	override def hasOrder = false
}