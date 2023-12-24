package net.noresttherein.sugar.collections

import scala.collection.IterableFactory

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}




object ZigZagSpec extends UntaggedSeqProps[ZigZag]("ZigZag", ZigZag) {
	override def knowsSize = true

	sealed abstract class Build[+E] {
		def apply() :ZigZag[E]
		def expect :Seq[E]
	}
	case object Empty extends Build[Nothing] {
		override def apply() = ZigZag.empty
		override def expect = Vector.empty
	}
	case class Append[E](prefix :Build[E], suffix :E) extends Build[E] {
		override def apply() = prefix() appended suffix
		override def expect = prefix.expect :+ suffix
	}
	case class Prepend[E](prefix :E, suffix :Build[E]) extends Build[E] {
		override def apply() = suffix() prepended prefix
		override def expect = prefix +: suffix.expect
	}
	case class AppendedAll[E](prefix :Build[E], suffix :Build[E]) extends Build[E] {
		override def apply() = prefix() appendedAll suffix()
		override def expect = prefix.expect :++ suffix.expect
	}
	case class PrependedAll[E](prefix :Build[E], suffix :Build[E]) extends Build[E] {
		override def apply() = suffix() prependedAll prefix()
		override def expect = prefix.expect ++: suffix.expect
	}

	implicit def arbitraryZigZagBuild[E :Arbitrary] :Arbitrary[Build[E]] = Arbitrary {
		class ZigZagGen {
			val zz :Gen[Build[E]] = Gen.frequency(
				4 ->Gen.const(Empty),
				2 -> Gen.lzy(for { prefix <- zigzag; suffix <- Arbitrary.arbitrary[E] } yield Append(prefix, suffix)),
				2 -> Gen.lzy(for { prefix <- Arbitrary.arbitrary[E]; suffix <- zigzag } yield Prepend(prefix, suffix)),
				1 -> Gen.lzy(for { prefix <- zigzag; suffix <- zigzag } yield AppendedAll(prefix, suffix)),
				1 -> Gen.lzy(for { prefix <- zigzag; suffix <- zigzag } yield PrependedAll(prefix, suffix)),
			)
			def zigzag = zz
		}
		(new ZigZagGen).zigzag
	}

	property("build") = forAll { template :Build[Int] => validate(template.expect, template()) }
}
