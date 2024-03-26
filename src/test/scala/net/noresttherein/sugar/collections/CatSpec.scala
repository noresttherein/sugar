package net.noresttherein.sugar.collections

import scala.collection.{IterableFactory, immutable}
import scala.reflect.ClassTag

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}

import net.noresttherein.sugar.collections.IterableProps.{Dummy, Filter, FlatMap, Fold, FoldSide, Map}




//Todo: make it somehow PatchingProps. It's problematic, because GenericIterableProps is invariant,
// and can't be made covariant even with cheating, because it has methods accepting and returning invariant classes,
// such as ClassTag[Cat[Int]]. We'd need to make PatchingProps completely separate, but it depends on
// test methods.
object CatSpec extends UntaggedSeqProps[Seq](Cat)/* with PatchingProps[Cat, collection.Seq, Dummy]*/ {
	override def knowsSize = false

	//Cat.map and others return a generic Seq, so we don't want to test a List - just verify equality.
	protected override def props[T, F, M, FM](expect :collection.Seq[T], result :Seq[T])
	                                         (implicit arbitrary :Arbitrary[T], ev :E[T], tag :ClassTag[T],
	                                                   filt :Filter[T], fldA :FoldSide[F, T], evf :E[F], fld :Fold[T],
	                                                   mp :Map[T, M], evm :E[M],
	                                                   fmap :FlatMap[T, FM], evfm :E[FM]) :Seq[Prop] =
		result match {
			case cat :Cat[T] => super.props(expect, result to Cat)
			case _           => Seq()
		}

	sealed abstract class Build[+E] {
		def apply() :Cat[E]
		def expect :Seq[E]
	}
	case object Empty extends Build[Nothing] {
		override def expect = Vector.empty
		override def apply() = Cat.empty
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
				2 -> Gen.lzy(for { prefix <- cat; suffix <- Arbitrary.arbitrary[E] } yield Append(prefix, suffix)),
				2 -> Gen.lzy(for { prefix <- Arbitrary.arbitrary[E]; suffix <- cat } yield Prepend(prefix, suffix)),
				1 -> Gen.lzy(for { prefix <- cat; suffix <- cat } yield AppendedAll(prefix, suffix)),
				1 -> Gen.lzy(for { prefix <- cat; suffix <- cat } yield PrependedAll(prefix, suffix)),
			)
			def cat = zz
		}
		(new CatGen).cat
	}

	property("build") = forAll { template :Build[Int] => test(template.expect, template()) }
}
