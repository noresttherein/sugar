package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop._
import org.scalacheck.util.{Buildable, ConsoleReporter, Pretty}
import net.noresttherein.sugar.extensions.{IterableExtension, SeqExtension}
import net.noresttherein.sugar.numeric.globalRandom
import net.noresttherein.sugar.testing.scalacheck.extensions.LazyExtension
import net.noresttherein.sugar.typist.ConvertibleTo
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.Got




object SeqExtensionSpec extends Properties("SeqExtension") {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)

	import typeClasses._

	def seqProperty[X :ClassTag :Arbitrary :Shrink :ConvertibleTo[Pretty]#T](prop :Seq[X] => Prop) :Prop =
		forAll { seq :List[X] => prop(seq) :| "List" } &&
			forAll { seq :ArraySeq[X] => prop(seq) :| "ArraySeq" } &&
			forAll { vec :Vector[X] => prop(vec) :| "Vector" } &&
			forAll { slice :IRefArraySlice[X] => prop(slice) :| "ArraySlice" }

	def lazyProperty[X :ClassTag :Arbitrary :Shrink :ConvertibleTo[Pretty]#T](prop :Seq[X] => Prop) :Prop =
		seqProperty[X](prop) && {
			def x() = Arbitrary.arbitrary[X].sample.get
			var i    = 0
			val list = { i += 1; x() } #:: { i += 1; x() } #:: { i += 1; x() } #:: LazyList.empty
			prop(list)
			(i ?= 0) :| "LazyList(1, 2, 3) had " + i + " evaluated elements" && prop(list) :| "LazyList"
		}

	property("isSorted") = seqProperty { seq :Seq[Int] =>
		val sorted = seq.sorted
		(seq.isSorted ?= (seq == sorted)) && Prop(sorted.isSorted) :| "sorted"
	}
	property("isSortedBy") = seqProperty { seq :Seq[Int] =>
		val sorted = seq.sortBy(x => x * x)
		(seq.isSortedBy(x => x * x) ?= (seq == sorted)) && Prop(sorted.isSortedBy(x => x * x)) :| "sorted"
	}
	property("isSortedWith") = seqProperty { seq :Seq[Int] =>
		val sorted = seq.sortBy(x => x * x) //seq.sorted(Ordering.by((x :Int) => x * x))
		((seq.isSortedWith((x, y) => x * x <= y * y) ?=
			(seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 * pair._1 <= pair._2 * pair._2))
		) && Prop(sorted.isSortedWith((x, y) => x * x <= y * y))) :| "sorted: " + sorted
	}
	property("isIncreasing") = seqProperty { seq :Seq[Int] =>
		val sorted = seq.sorted
		((seq.isIncreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 < pair._2))) &&
			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isIncreasing)) :| "sorted: " + sorted
	}
	property("isDecreasing") = seqProperty { seq :Seq[Int] =>
		val sorted = seq.sortWith((x, y) => x > y)
		((seq.isDecreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 > pair._2))) &&
			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isDecreasing)) :| "sorted: " + sorted
	}

	property("shuffle") = seqProperty { seq :Seq[Int] => seq.shuffle.sorted =? seq.sorted }

	private def lazyIndexProperty(idx :Int, knownDeltaSizeLimit :Int = -1, deltaSizeLimit :Int = -1)
	                               (f :Seq[Int] => (Seq[Int], Seq[Int])) =
		forAll { seq :Vector[Int] =>
			val list = LazyList.from(seq)
			if (idx < 0 || idx > knownDeltaSizeLimit && list.knownSize == 0)
				f(list).throws[IndexOutOfBoundsException]
			else if (idx > seq.size + deltaSizeLimit) {
				val (result, _) = f(list) //checks that it doesn't throw an exception here
				Vector.from(result).throws[IndexOutOfBoundsException]
			} else {
				var x = 0
				val list = { x += 1; 42 } #:: { x += 1; 44 } #:: LazyList.from(seq)
				val (result, expect) = f(list)
				(x ?= 0) :| "LazyList.removed evaluated before actual access" && (result ?= expect)
			}
		} :| "LazyList"

//	property("updated(first, second, rest*)") = seqProperty { seq :Seq[Int] =>
//		seqProperty { patch :Seq[Int] =>
//			forAll { (x :Int, y :Int, index :Int) =>
//				if (index < 0 || index + patch.length + 2 > seq.length)
//					seq.updated(index, x, y, patch :_*).throws[IndexOutOfBoundsException]
//				else
//					seq.updated(index, x, y, patch :_*) ?=
//						seq.take(index) :+ x :+ y :++ patch :++ seq.drop(index + 2 + patch.length)
//			}
//		}
//	}
	//permissive version
//	property("updatedAll") = forAll { index :Int =>
//		seqProperty { patch :Seq[Int] =>
//			seqProperty { seq :Seq[Int] =>
//				val overlap =
//					if (index < 0) patch.slice(-index, -index + seq.length)
//					else patch.take(seq.length - index)
//				seq.updatedAll(index, patch) ?=
//					seq.take(index) :++ overlap :++ seq.drop(index + patch.length)
//			}
//		}
//	}

//	property("inserted") = seqProperty { seq :Seq[Int] =>
//		forAll { (i :Int, x :Int) =>
//			val index = math.min(seq.length, math.max(i, 0))
//			seq.inserted(index, x) ?= seq.take(index) :+ x :++ seq.drop(index)
//		}
//	}
//	property("inserted(first, second, rest*)") = seqProperty { seq :Seq[Int] =>
//		seqProperty { patch :Seq[Int] =>
//			forAll { (x :Int, y :Int, index :Int) =>
//				seq.inserted(index, x, y, patch :_*) ?=
//					seq.take(index) :+ x :+ y :++ patch :++ seq.drop(index + 2 + patch.length)
//			}
//		}
//	}
//	property("insertedAll") =  seqProperty { seq :Seq[Int] =>
//		seqProperty { patch :Seq[Int] =>
//			forAll { index :Int =>
//				seq.insertedAll(index, patch) ?=
//					seq.take(index) :++ patch :++ seq.drop(index + patch.length)
//			}
//		}
//	}

	property("updatedAll") = forAll { index :Int =>
		seqProperty { patch :Seq[Int] =>
			def prop(seq :Seq[Int]) =
				seq.updatedAll(index, patch) -> (seq.take(index) ++ patch ++ seq.drop(index + patch.size))
			seqProperty { seq :Seq[Int] =>
				if (index < 0 || index > seq.length - patch.length)
					seq.updatedAll(index, patch).throws[IndexOutOfBoundsException]
				else {
					val (result, expect) = prop(seq)
					result ?= expect
				}
			} && lazyIndexProperty(index, if (patch.knownSize >= 0) -patch.knownSize else 0, -patch.size)(prop)
		}
	}
	property("inserted") = forAll { (index :Int, value :Int) =>
		def prop(seq :Seq[Int]) =
			seq.inserted(index, value) -> (seq.take(index) ++: value +: seq.drop(index))
		seqProperty { seq :Seq[Int] =>
			if (index < 0 || index > seq.length)
				seq.inserted(index, value).throws[IndexOutOfBoundsException]
			else {
				val (result, expect) = prop(seq)
				result ?= expect
			}
		} && lazyIndexProperty(index, 0, 0)(prop)
	}
	property("inserted(index, first, second, rest*)") = forAll { (index :Int, first :Int, second :Int) =>
		seqProperty { rest :Seq[Int] =>
			def prop(seq :Seq[Int]) =
				seq.inserted(index, first, second, rest :_*) -> (
					seq.take(index) ++: first +: second +: rest ++: seq.drop(index)
				)
			seqProperty { seq :Seq[Int] =>
				if (index < 0 || index > seq.length)
					seq.inserted(index, first, second, rest :_*).throws[IndexOutOfBoundsException]
				else {
					val (result, expect) = prop(seq)
					result ?= expect
				}
			} && lazyIndexProperty(index, 0, 0)(prop)
		}
	}
	property("insertedAll") = forAll { (index :Int) =>
		seqProperty { patch :Seq[Int] =>
			def prop(seq :Seq[Int]) =
				seq.insertedAll(index, patch) -> (seq.take(index) ++: patch ++: seq.drop(index))
			seqProperty { seq :Seq[Int] =>
				if (index < 0 || index > seq.length)
					seq.insertedAll(index, patch).throws[IndexOutOfBoundsException]
				else {
					val (result, expect) = prop(seq)
					result ?= expect
				}
			} && lazyIndexProperty(index, 0, 0)(prop)
		}
	}
//	property("appended(first, second, rest*)") = seqProperty { seq :Seq[Int] =>
//		seqProperty { patch :Seq[Int] =>
//			forAll { (x :Int, y :Int) =>
//				seq.appended(x, y, patch :_*) ?= seq.appendedAll(x +: y +: patch)
//			}
//		}
//	}
//	property("prepended(first, second, rest*)") = seqProperty { seq :Seq[Int] =>
//		seqProperty { patch :Seq[Int] =>
//			forAll { (x :Int, y :Int) =>
//				seq.prepended(x, y, patch :_*) ?= seq.prependedAll(x +: y +: patch)
//			}
//		}
//	}


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
		if (expect < 0) test(seq).throws[NoSuchElementException]
		else test(seq) ?= expect

//	private def indexOfSlice(seq :Seq[Int], from :Int, slice :Seq[Int]) :Int =
//		if (from == Int.Ma)

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
			val from = if (i == Int.MinValue) 0 else i.abs
			sureIndexOfProp(seq, seq.indexOfSlice(x, from))(_.sureIndexOfSlice(x, from))
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
