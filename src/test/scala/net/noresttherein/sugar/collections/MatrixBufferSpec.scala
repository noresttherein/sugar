package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{Buildable, ConsoleReporter}




object MatrixBufferSpec extends Properties("MatrixBuffer") with BufferProps[TestMatrixBuffer, Dummy]
//	extends UntaggedSeqProps[MatrixBuffer]("MatrixBuffer", ShrinkingMatrixBuffer)
//	   with BufferProps[MatrixBuffer, Dummy]
{
	protected override def minSuccessfulTests :Int = 1000
	override val parameters = overrideParameters(Test.Parameters.default)
	val Buff = TestMatrixBuffer

	implicit protected override def intEvidence :Dummy[Int] = IterableProps.dummy

	implicit override def buildableChecked[T :Dummy] :Buildable[T, Buff[T]] = new Buildable[T, Buff[T]] {
		override def builder :Builder[T, Buff[T]] = Buff.newBuilder[T]
	}

	//todo: cyclicCopyToArray test.
/*
	property("iterableFactory.from") = forAll { (xs :Seq[Int]) =>
		if (Buff.from(xs) != xs)
			Console.err.println("Arg: " + xs + "\nResult: " + Buff.from(xs))
		upcast(Buff.from(xs)) ?= xs
	}
*/

	property("mutations") = new BufferCommands[Int](Buff.from(_ :Seq[Int])).property()
}
