package net.noresttherein.sugar.collections

import net.noresttherein.sugar.extensions.{IterableExtension, SeqExtension}
import net.noresttherein.sugar.numeric.globalRandom
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.Got
import org.scalacheck.{Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.ConsoleReporter





object SeqExtensionSpec extends Properties("SeqExtension") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140))

	property("isSorted") = forAll { seq :Seq[Int] =>
		val sorted = seq.sorted
		(seq.isSorted ?= (seq == sorted)) && Prop(sorted.isSorted) :| "sorted"
	}
	property("isSortedBy") = forAll { seq :Seq[Int] =>
		val sorted = seq.sortBy(x => x * x)
		(seq.isSortedBy(x => x * x) ?= (seq == sorted)) && Prop(sorted.isSortedBy(x => x * x)) :| "sorted"
	}
	property("isSortedWith") = forAll { seq :Seq[Int] =>
		val sorted = seq.sortBy(x => x * x) //seq.sorted(Ordering.by((x :Int) => x * x))
		((seq.isSortedWith((x, y) => x * x <= y * y) ?=
			(seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 * pair._1 <= pair._2 * pair._2))
		) && Prop(sorted.isSortedWith((x, y) => x * x <= y * y))) :| "sorted: " + sorted
	}
	property("isIncreasing") = forAll { seq :Seq[Int] =>
		val sorted = seq.sorted
		((seq.isIncreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 < pair._2))) &&
			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isIncreasing)) :| "sorted: " + sorted
	}
	property("isDecreasing") = forAll { seq :Seq[Int] =>
		val sorted = seq.sortWith((x, y) => x > y)
		((seq.isDecreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 > pair._2))) &&
			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isDecreasing)) :| "sorted: " + sorted
	}

	property("shuffle") = forAll { seq :Seq[Int] => seq.shuffle.sorted =? seq.sorted }

	private def getIndex(i :Int) :Opt[Int] = Opt.when(i >= 0)(i)

	property("getIndexOf") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			seq.getIndexOf(x, i) ?= getIndex(seq.indexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.getIndexOf(x) ?= getIndex(seq.indexOf(x))) :| (x.toString + "@" + seq.indexOf(x))) &&
				(seq.getIndexOf(x, i) ?= Got(i)) :| s"#$i->$x"
		} :_*)
	}
	property("getLastIndexOf") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			seq.getLastIndexOf(x, i) ?= getIndex(seq.lastIndexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.getLastIndexOf(x) ?= getIndex(seq.lastIndexOf(x))) :| (x.toString + "@" + seq.lastIndexOf(x))) &&
				(seq.getLastIndexOf(x, i) ?= Got(i)) :| s"$x<-#$i"
		} :_*)
	}
	property("getIndexWhere") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			seq.getIndexWhere(_ == x, i) ?= getIndex(seq.indexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.getIndexWhere(_ == x) ?= getIndex(seq.indexOf(x))) :| (x.toString + "@" + seq.indexOf(x))) &&
				(seq.getIndexWhere(_ == x, i) ?= Got(i)) :| s"#$i->$x"
		} :_*)
	}
	property("getLastIndexWhere") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			seq.getLastIndexWhere(_ == x, i) ?= getIndex(seq.lastIndexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.getLastIndexWhere(_ == x) ?= getIndex(seq.lastIndexOf(x))) :| (x.toString + "@" + seq.lastIndexOf(x))) &&
				(seq.getLastIndexWhere(_ == x, i) ?= Got(i)) :| s"$x<-#$i"
		} :_*)
	}
	property("getIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.indexOfSlice(slice)
				((seq.getIndexOfSlice(slice) ?= getIndex(i)) :| slice.toString + "@" + i) &&
					((seq.getIndexOfSlice(slice, from) ?= Got(from)) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			seq.getIndexOfSlice(x, i & 0xffff) ?= getIndex(seq.indexOfSlice(x, i & 0xffff))
		}
	}
	property("getLastIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.lastIndexOfSlice(slice)
				((seq.getLastIndexOfSlice(slice) ?= getIndex(i)) :| slice.toString + "@" + i) &&
					((seq.getLastIndexOfSlice(slice, from) ?= Got(from)) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Short) =>
			seq.getLastIndexOfSlice(x, i & 0xffff) ?= getIndex(seq.lastIndexOfSlice(x, i & 0xffff))
		}
	}

	private def sureIndexOfProp(seq :Seq[Int], expect :Int)(test :Seq[Int] => Int) :Prop =
		if (expect < 0) Prop(throws(classOf[NoSuchElementException])(test(seq))) :| "missing"
		else (test(seq) ?= expect)

	property("sureIndexOf") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			sureIndexOfProp(seq, seq.indexOf(x, i))(_.sureIndexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.sureIndexOf(x) ?= seq.indexOf(x)) :| x.toString + "@" + seq.indexOf(x)) &&
				(seq.sureIndexOf(x, i) ?= i) :| s"#$i->$x"
		}:_*)
	}
	property("sureLastIndexOf") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			sureIndexOfProp(seq, seq.lastIndexOf(x, i))(_.sureLastIndexOf(x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.sureLastIndexOf(x) ?= seq.lastIndexOf(x)) :| x.toString + "@" + seq.lastIndexOf(x)) &&
				(seq.sureLastIndexOf(x, i) ?= i) :| s"$x<-#$i"
		}:_*)
	}
	property("sureIndexWhere") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			sureIndexOfProp(seq, seq.indexOf(x, i))(_.sureIndexWhere(_ == x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.sureIndexWhere(_ == x) ?= seq.indexOf(x)) :| x.toString + "@" + seq.indexOf(x)) &&
				(seq.sureIndexWhere(_ == x, i) ?= i) :| s"#$i->$x"
		}:_*)
	}
	property("sureLastIndexWhere") = forAll { (seq :Seq[Int]) =>
		forAll { (x :Int, i :Int) =>
			sureIndexOfProp(seq, seq.lastIndexOf(x, i))(_.sureLastIndexWhere(_ == x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.sureLastIndexWhere(_ == x) ?= seq.lastIndexOf(x)) :| x.toString + "@" + seq.lastIndexOf(x)) &&
				(seq.sureLastIndexWhere(_ == x, i) ?= i) :| s"$x<-#$i"
		}:_*)
	}
	property("sureIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.lastIndexOfSlice(slice)
				(sureIndexOfProp(seq, i)(_.sureLastIndexOfSlice(slice)) :| slice.toString + "@" + i) &&
					((seq.sureIndexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Int) =>
			sureIndexOfProp(seq, seq.indexOfSlice(x, i.abs))(_.sureIndexOfSlice(x, i.abs))
		}
	}
	property("sureLastIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.lastIndexOfSlice(slice)
				(sureIndexOfProp(seq, i)(_.sureLastIndexOfSlice(slice)) :| slice.toString + "@" + i) &&
					((seq.sureIndexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Int) =>
			sureIndexOfProp(seq, seq.lastIndexOfSlice(x, i.abs))(_.sureLastIndexOfSlice(x, i.abs))
		}
	}

	private def indexOrThrowProperty(seq :Seq[Int], expect :Int)(test :Seq[Int] => Int) :Prop =
		if (expect < 0) Prop(throws(classOf[IllegalArgumentException])(test(seq))) :| "missing"
		else test(seq) ?= expect

	property("indexOrThrow") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			indexOrThrowProperty(seq, seq.indexOf(x, i))(_.indexOrThrow[IllegalArgumentException](x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.indexOrThrow[IllegalArgumentException](x) ?= seq.indexOf(x)) :| s"$x@${seq.indexOf(x)}") &&
				(seq.indexOrThrow[IllegalArgumentException](x, i) ?= i) :| s"#$i->$x"
		} :_*)
	}
	property("lastIndexOrThrow") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			indexOrThrowProperty(seq, seq.lastIndexOf(x, i))(_.lastIndexOrThrow[IllegalArgumentException](x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.lastIndexOrThrow[IllegalArgumentException](x) ?= seq.lastIndexOf(x)) :| s"$x@${seq.lastIndexOf(x)}") &&
				(seq.lastIndexOrThrow[IllegalArgumentException](x, i) ?= i) :| s"$x<-#$i"
		} :_*)
	}
	property("indexWhereOrThrow") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			indexOrThrowProperty(seq, seq.indexOf(x, i))(_.indexWhereOrThrow[IllegalArgumentException](_ == x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.indexWhereOrThrow[IllegalArgumentException](_ == x) ?= seq.indexOf(x)) :| s"$x@${seq.indexOf(x)}") &&
				(seq.indexWhereOrThrow[IllegalArgumentException](_ == x, i) ?= i) :| s"$i->$x"
		} :_*)
	}
	property("lastIndexWhereThrow") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			indexOrThrowProperty(seq, seq.lastIndexOf(x, i))(_.lastIndexWhereOrThrow[IllegalArgumentException](_ == x, i))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.lastIndexWhereOrThrow[IllegalArgumentException](_ == x) ?= seq.lastIndexOf(x)) :| s"$x@${seq.lastIndexOf(x)}") &&
				(seq.lastIndexWhereOrThrow[IllegalArgumentException](_ == x, i) ?= i) :|  s"$x<-$i"
		} :_*)
	}

	private def assertIndexProperty(seq :Seq[Int], expect :Int)(test :Seq[Int] => Int) :Prop =
		if (expect < 0) Prop(throws(classOf[AssertionError])(test(seq))) :| "missing"
		else test(seq) ?= expect

	property("assertIndexOf") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			assertIndexProperty(seq, seq.indexOf(x, i))(_.assertIndexOf(x, i, "fail"))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.assertIndexOf(x, "fail") ?= seq.indexOf(x)) :| s"$x@${seq.indexOf(x)}") &&
				(seq.assertIndexOf(x, i, "fail") ?= i) :| s"#$i->$x"
			} :_*)
	}
	property("assertLastIndexOf") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			assertIndexProperty(seq, seq.lastIndexOf(x, i))(_.assertLastIndexOf(x, i, "fail"))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.assertLastIndexOf(x, "fail") ?= seq.lastIndexOf(x)) :| s"$x@${seq.lastIndexOf(x)}") &&
				(seq.assertLastIndexOf(x, i, "fail") ?= i) :| s"$x<-#$i"
		} :_*)
	}
	property("assertIndexWhere") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			assertIndexProperty(seq, seq.indexOf(x, i))(_.assertIndexWhere(_ == x, i, "fail"))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.assertIndexWhere(_ == x, "fail") ?= seq.indexOf(x)) :| s"$x@${seq.indexOf(x)}") &&
				(seq.assertIndexWhere(_ == x, i, "fail") ?= i) :| s"#$i->$x"
		} :_*)
	}
	property("assertLastIndexWhere") = forAll { seq :Seq[Int] =>
		forAll { (x :Int, i :Int) =>
			assertIndexProperty(seq, seq.lastIndexOf(x, i))(_.assertLastIndexWhere(_ == x, i, "fail"))
		} && all(seq.mapWithIndex { (x, i) =>
			((seq.assertLastIndexWhere(_ == x, "fail") ?= seq.lastIndexOf(x)) :| s"$x@${seq.lastIndexOf(x)}") &&
				(seq.assertLastIndexWhere(_ == x, i, "fail") ?= i) :| s"$x<-#$i"
		} :_*)
	}
	property("assertIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.indexOfSlice(slice)
				(assertIndexProperty(seq, i)(_.assertIndexOfSlice(slice)) :| slice.toString + "@" + i) &&
					((seq.assertIndexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Int) =>
			assertIndexProperty(seq, seq.indexOfSlice(x, i.abs))(_.assertIndexOfSlice(x, i.abs))
		}
	}
	property("assertLastIndexOfSlice") = forAll { (seq :Vector[Int]) =>
		all(
			(for {
				from <- seq.indices
				until <- from to seq.length
			} yield {
				val slice = seq.slice(from, until)
				val i = seq.lastIndexOfSlice(slice)
				(assertIndexProperty(seq, i)(_.assertLastIndexOfSlice(slice)) :| slice.toString + "@" + i) &&
					((seq.assertLastIndexOfSlice(slice, from) ?= from) :| "[" + from + ", " + until + ")@" + from)
			})
		:_*) && forAll { (x :Seq[Int], i :Int) =>
			assertIndexProperty(seq, seq.indexOfSlice(x, i.abs))(_.assertIndexOfSlice(x, i.abs))
		}
	}



	property("subseqOf") = forAll { seq :Seq[Int] =>
		Prop(seq.filter(_ % 3 == 1) subseqOf seq)
	}
}
