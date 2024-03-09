package net.noresttherein.sugar.collections

import scala.collection.IterableFactory

import net.noresttherein.sugar.collections.IterableProps.Dummy
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}




object CatSpec extends UntaggedSeqProps[Cat](Cat) with SugaredSeqProps[Cat, Dummy] {
	override def knowsSize = true

	sealed abstract class Build[+E] {
		def apply() :Cat[E]
		def expect :Seq[E]
	}
	case object Empty extends Build[Nothing] {
		override def apply() = Cat.empty
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

	implicit def arbitraryCatBuild[E :Arbitrary] :Arbitrary[Build[E]] = Arbitrary {
		class CatGen {
			val zz :Gen[Build[E]] = Gen.frequency(
				4 -> Gen.const(Empty),
				2 -> Gen.lzy(for {prefix <- cat; suffix <- Arbitrary.arbitrary[E]} yield Append(prefix, suffix)),
				2 -> Gen.lzy(for { prefix <- Arbitrary.arbitrary[E]; suffix <- cat} yield Prepend(prefix, suffix)),
				1 -> Gen.lzy(for {prefix <- cat; suffix <- cat} yield AppendedAll(prefix, suffix)),
				1 -> Gen.lzy(for {prefix <- cat; suffix <- cat} yield PrependedAll(prefix, suffix)),
			)
			def cat = zz
		}
		(new CatGen).cat
	}

	property("build") = forAll { template :Build[Int] => test(template.expect, template()) }
}
