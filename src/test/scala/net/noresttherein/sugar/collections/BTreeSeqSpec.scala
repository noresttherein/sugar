package net.noresttherein.sugar.collections

import scala.collection.IterableFactory
import scala.collection.mutable.Builder

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.Prop.{AnyOperators, all, forAll}
import org.scalacheck.commands.Commands
import org.scalacheck.util.{Buildable, ConsoleReporter}
import net.noresttherein.sugar.testing.scalacheck.extensions.{LazyExtension, PropExtension}



/**
  * @author Marcin Mościcki
  */
object BTreeSeqSpec extends SeqProps[BTreeSeq]("BTreeSeq") {
	protected override def checkedFactory :IterableFactory[BTreeSeq] = BTreeSeq
	protected override def knowsSize = true

	property("removed") = test { (seq :Seq[Int], tree :BTreeSeq[Int]) =>
		forAll { i :Int =>
			if (i < 0 || i >= seq.length) tree.removed(i).throws[IndexOutOfBoundsException]
			else validate(seq.take(i) ++ seq.drop(i + 1), tree.removed(i))
		}
	}
	property("inserted") = test { (seq :Seq[Int], tree :BTreeSeq[Int]) =>
		forAll { x :Int =>
			(0 to seq.length).map { i =>
				val (prefix, suffix) = seq.splitAt(i)
				(prefix ++: x +: suffix) =? tree.inserted(i, x) label i.toString
			}.fold(Prop.passed)(_ && _) &&
				tree.inserted(-1, x).throws[IndexOutOfBoundsException] :| "-1" &&
				tree.inserted(seq.length + 1, x).throws[IndexOutOfBoundsException] :| "length + 1"
		}
	}
}
