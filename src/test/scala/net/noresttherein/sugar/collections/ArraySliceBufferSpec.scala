package net.noresttherein.sugar.collections

import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.IterableProps.Dummy
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.util.Buildable
import org.scalacheck.{Properties, Test}




private object ArraySliceBufferSpec
	extends UntaggedSeqProps[ArraySliceBuffer](ArraySliceBuffer) with AliasingBufferProps[ArraySliceBuffer]
{
	//The easiest way to test is to temporarily implement toString so that it shows offset..(offset+length)/array.length.
	//Then simply create a buffer with the specified initial size, add offset+length elements, and drop offset elements.

	override val parameters = overrideParameters(Test.Parameters.default)
	override val Buff = ArraySliceBuffer

	implicit override def intEvidence :Dummy[Int] = new Dummy
	implicit override def buildableChecked[T :Dummy] :Buildable[T, Buff[T]] =
		super.buildableChecked
}



