import scala.collection.Stepper
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.classTag

import net.noresttherein.sugar.collections.{OrderedItems, PassedArray, Ranking, RefArray, RefArraySlice}
import net.noresttherein.sugar.extensions.{ArrayExtension, IterableExtension, IteratorExtension, SeqExtension, SeqFactoryExtension, StepperObjectExtension, classNameMethods}
import net.noresttherein.sugar.reflect.RuntimeType
import net.noresttherein.sugar.reflect.RuntimeType.Specialized

object Playground extends App {
	var i = 0
	var step = Stepper.ofInt("X", 1, 0)
	println(Stepper.ofInt("X").iterator.toSeq)
	println(Stepper.ofInt("X").javaIterator.asScala.toSeq)
}