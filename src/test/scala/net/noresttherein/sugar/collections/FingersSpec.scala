package net.noresttherein.sugar.collections

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.Test
import org.scalacheck.util.ConsoleReporter




object FingersSpec
	extends UntaggedSeqProps[TestFingers]("Fingers", TestFingers)
	   with SugaredIterableProps[TestFingers, collection.Seq, Dummy]
	   with PatchingProps[TestFingers, collection.Seq, Dummy]
{
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
//		super.overrideParameters(p).withMaxSize(500)
		p.withTestCallback(ConsoleReporter(4, 100000)).withMinSuccessfulTests(500).withMaxSize(500)

	override def knowsSize = true
}
