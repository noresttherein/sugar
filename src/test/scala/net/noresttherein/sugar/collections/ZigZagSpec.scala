package net.noresttherein.sugar.collections

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.commands.Commands
import scala.collection.{mutable, IterableFactory, Stepper}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.ClassTag
import scala.util.{Success, Try}

import net.noresttherein.sugar.collections.IterableProps.{filter, flatMap, fold, foldLeft, foldRight, foldZero, map, value, Filter, FlatMap, Fold, FoldSide, Map}
import net.noresttherein.sugar.extensions.{castTypeParam, classNameExtension, classNameMethods}
import net.noresttherein.sugar.tuples.extensions.tuple2Extension
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}
import org.scalacheck.Prop.{all, forAll, AnyOperators}
import org.scalacheck.util.{Buildable, ConsoleReporter}




object ZigZagSpec extends SeqProps[ZigZag]("ZigZag") {
//	override def referenceFactory = List
	override def checkedFactory :IterableFactory[ZigZag] = ZigZag
	override def knowsSize = true
}
