package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import org.scalacheck.Prop._
import org.scalacheck.util.{ConsoleReporter, Pretty}
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink, Test}
import net.noresttherein.sugar.??!
import net.noresttherein.sugar.collections.RefArray.RefArrayToSeq
import net.noresttherein.sugar.collections.extensions.{ArrayExtension, ArrayLikeExtension, ArrayObjectExtension, IterableExtension, SeqExtension, immutableIndexedSeqObjectExtension}
import net.noresttherein.sugar.extensions.{SeqFactoryExtension, castTypeParamMethods, classNameMethods}
import net.noresttherein.sugar.reflect.prettyprint.localNameOf
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, Prettify, PropExtension}
import net.noresttherein.sugar.witness.DefaultValue
import net.noresttherein.sugar.witness.DefaultValue.default




abstract class ArrayTestingUtils(name :String) extends Properties(name) {
	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(2, 140)).withMinSuccessfulTests(1000)


	protected def any[X :Arbitrary] :X = Arbitrary.arbitrary[X].sample.get

	def forAllSeq[X :ClassTag :Arbitrary :Shrink :Prettify](prop :Seq[X] => Prop) :Prop =
		forAll { seq :List[X] => prop(seq) :| "List" } &&
			forAll { seq :Vector[X] => prop(seq) :| "Vector" } &&
			forAll { seq :ArraySeq[X] => prop(seq) :| "ArraySeq" } &&
			forAll { seq :IRefArraySlice[X] => prop(seq) :| "ArraySlice" }

	def forAllIterable[X :ClassTag :Arbitrary :Shrink :Prettify](prop :Iterable[X] => Prop) :Prop =
		forAllSeq(prop) && forAll { items :OrderedItems[X] => prop(items) :| "Iterable" }
//			forAll { seq :List[X] => prop(seq.iterator) :| "List.iterator" } &&
//			forAll { seq :ArraySeq[X] => prop(seq.iterator) :| "Array.iterator" }

	def forAllArrays(prop :GenericArrayProperty) :Prop =
		forAll { a :Array[Byte]    => prop(a) :| "Array[Byte]" lbl a.contentsString} &&
		forAll { a :Array[Short]   => prop(a) :| "Array[Short]" lbl a.contentsString } &&
		forAll { a :Array[Char]    => prop(a) :| "Array[Char]" lbl a.contentsString } &&
		forAll { a :Array[Int]     => prop(a) :| "Array[Int]" lbl a.contentsString } &&
		forAll { a :Array[Long]    => prop(a) :| "Array[Long]" lbl a.contentsString } &&
		forAll { a :Array[Float]   => prop(a) :| "Array[Float]" lbl a.contentsString } &&
		forAll { a :Array[Double]  => prop(a) :| "Array[Double]" lbl a.contentsString } &&
		forAll { a :Array[Boolean] => prop(a) :| "Array[Boolean]" lbl a.contentsString } &&
		forAll { a :Array[Unit]    => prop(a) :| "Array[Unit]" lbl a.contentsString } &&
		forAll { a :Array[String]  => prop(a) :| "Array[String]" lbl a.contentsString }

	def forAllArrays(property :String)(prop :GenericArrayProperty) :Unit =
		this.property(property) = forAllArrays(prop)

	protected def seq[A](array :Array[A]) :collection.Seq[A] = ArraySeq.unsafeWrapArray(array)


	trait GenericArrayProperty {
		def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop
	}

	abstract class ArrayProperty(val name :String) extends GenericArrayProperty {
		forAllArrays(name)(this)
	}

}






object ArrayExtensionSpec extends ArrayTestingUtils("ArrayExtension") {
	import typeClasses._


	property("segmentLength") = forAll { (start :Int, array :Array[Int]) =>
		val start0 = math.max(start, 0)
		var i = start0
		while (i < array.length && array(i) % 5 > 0)
			i += 1
		(array.segmentLength(_ % 5 > 0, start) ?= (i - start0)) lbl
			array.contentsString + ".segmentLength(_ % 5 > 0, " + start + ")"
	}

	new ArrayProperty("binarySearch(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop = {
			val sorted = array.sorted
			import Ordering.Implicits.infixOrderingOps
			forAll { (from :Int, until :Int) =>
				val validFrom  = from max 0 min array.length
				val validUntil = until max validFrom min array.length
//				val sorted = array.slice(from, until).sorted
//				val subject = array.view.take(from) ++ sorted ++ array.view.drop(math.max(from, until)) to Array
//				val slice = subject.slice(from, until)
				all(
					(validFrom until validUntil).map { i =>
						val x = sorted(i)
						val expect = sorted.indexOf(x, validFrom)
						val found = sorted.binarySearch(from, until, sorted(i))
						(found ?= expect) lbl s"searching for $x (at $i); found at $found, first occurrence at $expect"
					} :_*
				) && forAll { x :X =>
					val expect = sorted.indexOf(x, validFrom) match {
						case -1 => -1
						case  n if n >= validUntil => -1
						case  n => n
					}
					val found = sorted.binarySearch(from, until, x)
					val successor = -found - 1
					if (expect >= 0)
						(found ?= expect) lbl s"searching for $x found(?) at $found, first occurrence at $expect"
					else
						((found < 0) lbl
							s"should not find $x at $found"
						) && ((successor <= validUntil && successor >= validFrom) lbl
							s"search result $found out of range $from..$until"
						) && {
							val expect = sorted.indexWhere(_ >= x, validFrom) match {
								case -1 => validUntil
								case  n if n > validUntil => validUntil
								case  n => n
							}
							(successor ?= expect)
						}
				} lbl "searching in range " + validFrom + ".." + validUntil
			} lbl "sorted: " + seq(sorted)
		}
	}

	new ArrayProperty("mismatch") {
		def property[X](array1 :Array[X], from1 :Int, until1 :Int, array2 :Array[X], from2 :Int, until2 :Int) :Prop = {
			val slice1 = array1.slice(from1, until1)
			val slice2 = array2.slice(from2, until2)
			val expect = slice1.zip(slice2).indexWhere { case (left, right) => left != right } match {
				case -1 if slice1.length == slice2.length => -1
				case -1 => slice1.length min slice2.length
				case  n => n
			}
			val result = array1.mismatch(array2, from1, until1, from2, until2)
			(result ?= expect) lbl
				s"comparing ${array1.contentsString}[$from1..$until1]\n" +
				s"with      ${array2.contentsString}[$from2..$until2];\n" +
				s"expected mismatch at $expect, found at $result"
		}

		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				forAll { (split :Int, suffixLen :Byte) =>
					val validFrom  = math.max(0, math.min(from, array.length))
					val validUntil = math.max(validFrom, math.min(until, array.length))
					val validSuffixLen = suffixLen & 0xff
					val clippedUntil = validUntil - validSuffixLen
					val validSplit = split min array.length max 0
					val array2 =
						(any[X] +: array.view.slice(validFrom, validSplit)) ++
							ArraySeq.fill(validSuffixLen)(any[X]) to Array

					property(array, from, until, array, from, clippedUntil) &&
						property(array, from, clippedUntil, array, from, until) &&
						property(array, from, clippedUntil, array2, 1, validUntil - validFrom) &&
						property(array2, 1, array2.length, array, from, until)
				}
			}
	}


	//todo: test sameElements


	new ArrayProperty("fill") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) = {
			val x = any[X]
			array.fill(x)
			array.toSeq ?= IndexedSeq.const(array.length)(x)
		}
	}
	new ArrayProperty("fill(from, until, elem)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val x = any[X]
				val expect = array.toBuffer
				var i   = math.max(from, 0)
				val end = math.min(until, array.length)
				while (i < end) {
					expect(i) = x; i += 1
				}
				array.fill(from, until)(x)
				seq(array) ?= expect
			}
	}
	new ArrayProperty("clear") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) = {
			array.clear()
			seq(array) ?= Array.fill(array.length)(default[X])
		}
	}
	new ArrayProperty("clear(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val expect = array.toBuffer
				var i   = math.max(from, 0)
				val end = math.min(until, array.length)
				while (i < end) {
					expect(i) = default
					i += 1
				}
				array.clear(from, until)
				seq(array) ?= expect
			}
	}
	new ArrayProperty("clearIfRef") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) = {
			val copy = array.toSeq
			array.clearIfRef()
			if (array.isInstanceOf[Array[AnyRef]] && !array.isInstanceOf[Array[Unit]])
				(seq(array) :collection.Seq[Any]) ?= IndexedSeq.const(array.length)(null)
			else
				seq(array) ?= copy
		}
	}
	new ArrayProperty("clearIfRef(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val expect = array.toBuffer
				array.clearIfRef(from, until)
				if (array.isInstanceOf[Array[AnyRef]] && !array.isInstanceOf[Array[Unit]]) {
					var i   = math.max(from, 0)
					val end = math.min(until, array.length)
					while (i < end) {
						expect(i) = default
						i += 1
					}
				}
				seq(array) ?= expect
			}
	}


	new ArrayProperty("reverseInPlace") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) = {
			val reversed = seq(array).reverse
			array.reverseInPlace()
			seq(array) ?= reversed
		}
	}
	new ArrayProperty("reverseInPlace(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val expect =
					array.view.take(from) ++
						seq(array).slice(from, until).reverse ++
						array.drop(math.max(from, until)) to Array
				array.reverseInPlace(from, until)
				seq(array) ?= seq(expect)
			}
	}

	new ArrayProperty("reverse") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) = {
			val reversed = seq(array).reverse
			seq(array.reverse) ?= reversed
		}
	}
	new ArrayProperty("reverse(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val expect =
					array.view.take(from) ++
						seq(array).slice(from, until).reverse ++
						array.drop(math.max(from, until)) to Array
				seq(array.reverse(from, until)) ?= seq(expect)
			}
	}


	new ArrayProperty("rotateLeft") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { n :Int =>
				val expect =
					if (array.length == 0)
						Nil
					else {
						val x = n % array.length
						if (x >= 0) array.view.drop(x) ++ array.view.take(x) to ArraySeq
						else array.view.takeRight(-x) ++ array.view.dropRight(-x) to ArraySeq
					}
				array.rotateLeft(n)
				seq(array) ?= expect
			}
	}
	new ArrayProperty("rotateLeft(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int, n :Int) =>
				val start = math.max(0, math.min(from, array.length))
				val end   = math.max(start, math.min(until, array.length))
				val expect =
					if (end <= start)
						array to ArraySeq
					else {
						val x     = n % (end - start)
						val view  = array.view
						view.take(start) ++ (
							if (x >= 0) view.slice(start + x, end) ++ view.slice(start, start + x)
							else view.slice(end + x, end) ++ view.slice(start, end + x)
						) ++ view.drop(end) to ArraySeq
					}
				array.rotateLeft(from, until)(n)
				seq(array) ?= expect
			}
	}
	new ArrayProperty("rotateRight") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { n :Int =>
				val expect =
					if (array.length == 0)
						Nil
					else {
						val x = n % array.length
						if (x >= 0) array.view.takeRight(x) ++ array.view.dropRight(x) to ArraySeq
						else array.view.drop(-x) ++ array.view.take(-x) to ArraySeq
					}
				array.rotateRight(n)
				seq(array) ?= expect
			}
	}
	new ArrayProperty("rotateRight(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int, n :Int) =>
				val start = math.max(0, math.min(from, array.length))
				val end   = math.max(start, math.min(until, array.length))
				val expect =
					if (end <= start)
						array to ArraySeq
					else {
						val x    = n % (end - start)
						val view = array.view
						view.take(start) ++ (
							if (x >= 0) view.slice(end - x, end) ++ view.slice(start, end - x)
							else view.slice(start - x, end) ++ view.slice(start, start - x)
						) ++ view.drop(end) to ArraySeq
					}
				array.rotateRight(from, until)(n)
				seq(array) ?= expect
			}
	}

	new ArrayProperty("rotatedLeft") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { n :Int =>
				val expect =
					if (array.length == 0)
						Nil
					else {
						val x = n % array.length
						if (x >= 0) array.view.drop(x) ++ array.view.take(x) to ArraySeq
						else array.view.takeRight(-x) ++ array.view.dropRight(-x) to ArraySeq
					}
				seq(array.rotatedLeft(n)) ?= expect
			}
	}
	new ArrayProperty("rotatedLeft(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int, n :Int) =>
				val start = math.max(0, math.min(from, array.length))
				val end   = math.max(start, math.min(until, array.length))
				val expect =
					if (end <= start)
						array to ArraySeq
					else {
						val x     = n % (end - start)
						val view  = array.view
						view.take(start) ++ (
							if (x >= 0) view.slice(start + x, end) ++ view.slice(start, start + x)
							else view.slice(end + x, end) ++ view.slice(start, end + x)
						) ++ view.drop(end) to ArraySeq
					}
				seq(array.rotatedLeft(from, until)(n)) ?= expect
			}
	}
	new ArrayProperty("rotatedRight") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { n :Int =>
				val expect =
					if (array.length == 0)
						Nil
					else {
						val x = n % array.length
						if (x >= 0) array.view.takeRight(x) ++ array.view.dropRight(x) to ArraySeq
						else array.view.drop(-x) ++ array.view.take(-x) to ArraySeq
					}
				seq(array.rotatedRight(n)) ?= expect
			}
	}
	new ArrayProperty("rotatedRight(from, until)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int, n :Int) =>
				val start = math.max(0, math.min(from, array.length))
				val end   = math.max(start, math.min(until, array.length))
				val expect =
					if (end <= start)
						array to ArraySeq
					else {
						val x    = n % (end - start)
						val view = array.view
						view.take(start) ++ (
							if (x >= 0) view.slice(end - x, end) ++ view.slice(start, end - x)
							else view.slice(start - x, end) ++ view.slice(start, start - x)
						) ++ view.drop(end) to ArraySeq
					}
				seq(array.rotatedRight(from, until)(n)) ?= expect
			}
	}

//	forAllArrays("isSorted")(new GenericArrayProperty {
//		override def apply[X :ClassTag :Ordering](array :Array[X]) = {
//			val sorted = array.sorted
//			array.isSorted ?= (array sameElements sorted) && sorted.isSorted :| lbl "sorted: " + sorted.mkString
//		}
//	})
//	property("isSortedBy") = arrayProperty { seq :Seq[Int] =>
//		val sorted = seq.sortBy(x => x * x)
//		(seq.isSortedBy(x => x * x) ?= (seq == sorted)) && Prop(sorted.isSortedBy(x => x * x)) :| "sorted"
//	}
//	property("isSortedWith") = arrayProperty { seq :Seq[Int] =>
//		val sorted = seq.sortBy(x => x * x) //seq.sorted(Ordering.by((x :Int) => x * x))
//		((seq.isSortedWith((x, y) => x * x <= y * y) ?=
//			(seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 * pair._1 <= pair._2 * pair._2))
//		) && Prop(sorted.isSortedWith((x, y) => x * x <= y * y))) :| "sorted: " + sorted
//	}
//	property("isIncreasing") = arrayProperty { seq :Seq[Int] =>
//		val sorted = seq.sorted
//		((seq.isIncreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 < pair._2))) &&
//			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isIncreasing)) :| "sorted: " + sorted
//	}
//	property("isDecreasing") = arrayProperty { seq :Seq[Int] =>
//		val sorted = seq.sortWith((x, y) => x > y)
//		((seq.isDecreasing ?= (seq.sizeIs <= 1 || seq.zip(seq.tail).forall(pair => pair._1 > pair._2))) &&
//			Prop(sorted.toSet.size != sorted.length) || Prop(sorted.isDecreasing)) :| "sorted: " + sorted
//	}
//
//	property("shuffle") = arrayProperty { seq :Seq[Int] => seq.shuffle.sorted =? seq.sorted }


	new ArrayProperty("updateAll(Int, IterableOnce)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { index :Int =>
				forAllIterable { elems :Iterable[X] =>
					if (index < 0 || index > array.length - elems.size)
						array.updateAll(index, elems).throws[IndexOutOfBoundsException] &&
							array.updateAll(index, elems.iterator).throws[IndexOutOfBoundsException] :| "Iterator"
					else {
						val expect = array.view.take(index) ++ elems ++ array.view.drop(index + elems.size) to ArraySeq
						array.updateAll(index, elems)
						seq(array) ?= expect
					}
				}
			}
	}
	new ArrayProperty("updateAll(Int, ArrayLike)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (index :Int) =>
				(forAll { elems :Array[X] =>
					if (index < 0 || index > array.length - elems.length)
						array.updateAll(index, elems).throws[IndexOutOfBoundsException]
					else {
						val expect = array.view.take(index) ++ elems ++ array.view.drop(index + elems.length) to ArraySeq
						array.updateAll(index, elems)
						seq(array) ?= expect
					}
				} lbl "updateAll(" + index + ", Array[" + localNameOf[X] + "])") &&
				(forAll { src :Array[X] =>
					val elems = RefArray.copyOf(src)
					if (index < 0 || index > array.length - src.length)
						array.updateAll(index, elems).throws[IndexOutOfBoundsException]
					else {
						val expect = array.view.take(index) ++ src ++ array.view.drop(index + src.length) to ArraySeq
						array.updateAll(index, elems)
						seq(array) ?= expect
					}
				} lbl "updateAll(" + index + ", RefArray[" + localNameOf[X] + "])")
			}
	}
	new ArrayProperty("updateAll(Int, X, X, X*)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (index :Int) =>
				val first  = any[X]
				val second = any[X]
				forAllSeq { rest :Seq[X] =>
					val updated = rest.length + 2
					if (index < 0 || index > array.length - updated)
						array.updateAll(index, first, second, rest :_*).throws[IndexOutOfBoundsException]
					else {
						val view = array.view
						val expect =
							view.take(index) ++ Seq(first, second) ++ rest ++ array.drop(index + updated) to ArraySeq
						array.updateAll(index, first, second, rest :_*)
						seq(array) ?= expect
					}
				} lbl "first: " + first + ", second: " + second
			}
	}


	new GenericPatchProperty {
		override def methodName :String = "remove"
		override def argList :String = "(Int)"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { index :Int => patchProperty(array, Nil, index, 1)(array.removed(index)) }

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify](array :Array[X], index :Int, replaced :Int => Int) =
			??!
	}
	new ArrayProperty("removed(Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) =
			forAll { (from :Int, until :Int) =>
				val expect  = array.view.take(from) ++ array.view.drop(math.max(until, from)) to ArraySeq
				seq(array.removed(from, until)) ?= expect
			}
	}

	new UpdateProperty with PatchOneProperty {
		override def methodName = "updated"
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elem :Y, replaced :Int) :Array[Y] =
			array.updated(index, elem)
	}
	new UpdateProperty with PatchIterableOnceProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) =
			array.updatedAll(index, elems)
	}
	new UpdateProperty with PatchVarArgsProperty {
		override def patch[X, Y >: X :ClassTag]
		                  (array :Array[X], index :Int, first :Y, second :Y, rest :Seq[Y], replaced :Int) =
			array.updatedAll(index, first, second, rest :_*)	
	}
	new UpdateProperty with PatchArrayLikeProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) =
			array.updatedAll(index, elems)	
	}
	new UpdateProperty with PatchArrayProperty {
		override def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) =
			array.updatedAll(index, elems)
	}

	new InsertProperty with PatchOneProperty {
		override def methodName = "inserted"
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elem :Y, replaced :Int) :Array[Y] =
			array.inserted(index, elem)
	}
	new InsertProperty with PatchIterableOnceProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) =
			array.insertedAll(index, elems)
	}
	new InsertProperty with PatchVarArgsProperty {
		override def patch[X, Y >: X :ClassTag]
		                  (array :Array[X], index :Int, first :Y, second :Y, rest :Seq[Y], replaced :Int) =
			array.insertedAll(index, first, second, rest :_*)	
	}
	new InsertProperty with PatchArrayLikeProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) =
			array.insertedAll(index, elems)	
	}
	new InsertProperty with PatchArrayProperty {
		override def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) =
			array.insertedAll(index, elems)
	}

	new AppendProperty with PatchOneProperty {
		override def methodName = "appended"
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elem :Y, replaced :Int) :Array[Y] =
			array.appended(elem)
	}
	new AppendProperty with PatchIterableOnceProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) =
			array.appendedAll(elems)
	}
	new AppendProperty with PatchVarArgsProperty {
		override def patch[X, Y >: X :ClassTag]
		                  (array :Array[X], index :Int, first :Y, second :Y, rest :Seq[Y], replaced :Int) =
			array.appendedAll(first, second, rest :_*)	
	}
	new AppendProperty with PatchArrayLikeProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) =
			array.appendedAll(elems)	
	}
	new AppendProperty with PatchArrayProperty {
		override def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) =
			array.appendedAll(elems)
	}

	new PrependProperty with PatchOneProperty {
		override def methodName = "prepended"
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elem :Y, replaced :Int) =
			array.prepended(elem)
	}
	new PrependProperty with PatchIterableOnceProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) =
			array.prependedAll(elems)
	}
	new PrependProperty with PatchVarArgsProperty {
		override def patch[X, Y >: X :ClassTag]
			              (array :Array[X], index :Int, first :Y, second :Y, rest :Seq[Y], replaced :Int) =
			array.prependedAll(first, second, rest :_*)	
	}
	new PrependProperty with PatchArrayLikeProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) =
			array.prependedAll(elems)	
	}
	new PrependProperty with PatchArrayProperty {
		override def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) =
			array.prependedAll(elems)
	}

	new PatchProperty with PatchIterableOnceProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) =
			array.patch(index, elems, replaced)
	}
	new PatchProperty with PatchArrayLikeProperty {
		override def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) =
			array.patch(index, elems, replaced)
	}
	new PatchProperty with PatchArrayProperty {
		override def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) :Array[Y] =
			array.patch(index, elems, replaced)
	}


	trait GenericPatchProperty extends GenericArrayProperty {
		def name :String = methodName + argList
		def argList :String
		def methodName :String
		def apply[X :ClassTag :Arbitrary :Shrink :Prettify](array :Array[X], index :Int, replaced :Int => Int) :Prop

		forAllArrays(name)(this)

		def patchProperty[X, U >: X :ClassTag](array :Array[X], elems :IterableOnce[U], index :Int, replaced :Int)
		                                      (patch: => Array[U]) :Prop =
			if (index < 0 || index > array.length - replaced)
				patch.throws[IndexOutOfBoundsException]
			else {
				val expect = array.view.take(index) ++ elems ++ array.view.drop(index + replaced) to ArraySeq
				seq(patch) ?= expect
			}
	}

	trait InsertProperty extends GenericPatchProperty {
		override def methodName :String = "insertedAll"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { index :Int => apply(array, index, _ => 0) }
	}
	trait UpdateProperty extends GenericPatchProperty {
		override def methodName :String = "updatedAll"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { index :Int => apply(array, index, identity) }
	}
	trait AppendProperty extends GenericPatchProperty {
		override def methodName :String = "appendedAll"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			apply(array, array.length, _ => 0)
	}
	trait PrependProperty extends GenericPatchProperty {
		override def methodName :String = "prependedAll"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			apply(array, 0, _ => 0)
	}
	trait PatchProperty extends GenericPatchProperty {
		override def methodName :String = "patch"
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (index :Int, replaced :Int) =>
				apply(array, index, _ => replaced)
			}

		override def patchProperty[X, U >: X :ClassTag]
		                          (array :Array[X], elems :IterableOnce[U], index :Int, replaced :Int)
		                          (patch : => Array[U]) :Prop =
		{
			val index0 = math.max(index, 0)
			val replaced0 = math.min(Int.MaxValue - index0, math.max(replaced, 0))
			val expect = array.view.take(index0) ++ elems ++ array.view.drop(index0 + replaced0) to ArraySeq
			seq(patch) ?= expect
		}
	}


	private trait PatchOneProperty extends GenericPatchProperty {
		override def argList = "(X)"
		def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elem :Y, replaced :Int) :Array[Y]

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify]
			              (array :Array[X], index :Int, replaced :Int => Int) :Prop =
			patchOneProperty[X, X](array, index, replaced)(patch(_, index, _, _)) &&
				patchOneProperty[X, Any](array, index, replaced)(patch(_, index, _, _))

		def patchOneProperty[X, U >: X :ClassTag :Arbitrary :Shrink :Prettify]
		                    (array :Array[X], index :Int, replace :Int => Int)(patch :(Array[X], U, Int) => Array[U]) =
			forAll { elem :U =>
				patchProperty(array, elem::Nil, index, replace(1))(patch(array, elem, replace(1))) :| "patch: " + elem
			}
	}

	private trait PatchIterableOnceProperty extends GenericPatchProperty {
		override def argList = "(IterableOnce)"
		def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :IterableOnce[Y], replaced :Int) :Array[Y]

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify]
			              (array :Array[X], index :Int, replaced :Int => Int) :Prop =
			patchIterableOnceProperty[X, X](array, index, replaced)(patch(_, index, _, _)) &&
				patchIterableOnceProperty[X, Any](array, index, replaced)(patch(_, index, _, _))

		def patchIterableOnceProperty[X, U >: X :ClassTag :Arbitrary :Shrink :Prettify]
		                             (array :Array[X], index :Int, replace :Int => Int)
		                             (patch :(Array[X], IterableOnce[U], Int) => Array[U]) :Prop =
			forAllIterable { elems :Iterable[U] =>
				val replaced = replace(elems.size)
				(patchProperty(array, elems, index, replaced)(patch(array, elems, replaced)) lbl
					elems.mkString("patch: " + elems.localClassName + "[" + localNameOf[U] + "](", ", ", ")")) &&
				(patchProperty(array, elems.iterator, index, replaced)(patch(array, elems.iterator, replaced)) lbl
					elems.mkString("patch: Iterator[" + localNameOf[U] + "](", ", ", ")"))
			}
	}

	private trait PatchVarArgsProperty extends GenericPatchProperty {
		override def argList = "(X, X, X*)"
		def patch[X, Y >: X :ClassTag]
			     (array :Array[X], index :Int, first :Y, second :Y, rest :Seq[Y], replaced :Int) :Array[Y]

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify]
			              (array :Array[X], index :Int, replaced :Int => Int) :Prop =
			patchVarArgsProperty[X, X](array, index, replaced)(patch(_, index, _, _, _, _)) &&
				patchVarArgsProperty[X, Any](array, index, replaced)(patch(_, index, _, _, _, _))

		def patchVarArgsProperty[X, U >: X :ClassTag :Arbitrary :Shrink :Prettify]
	                            (array :Array[X], index :Int, replace :Int => Int)
	                            (patch :(Array[X], U, U, Seq[U], Int) => Array[U]) :Prop =
			forAll { (first :U, second :U) =>
				forAllSeq { rest :Seq[U] =>
					val elems = new Prepended2Seq(first, second, rest)
					val replaced = replace(elems.size)
					patchProperty(array, elems, index, replaced)(patch(array, first, second, rest, replaced)) lbl
						"[" + localNameOf[U] + "](" + first + ", " + second + ", " + rest + ")"
				}
			}
	}

	private trait PatchArrayLikeProperty extends GenericPatchProperty {
		override def argList = "(ArrayLike)"
		def patch[X, Y >: X :ClassTag](array :Array[X], index :Int, elems :ArrayLike[Y], replaced :Int) :Array[Y]

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify]
			              (array :Array[X], index :Int, replaced :Int => Int) :Prop =
			patchArrayLikeProperty[X, X](array, index, replaced)(patch(_, index, _, _)) &&
				patchArrayLikeProperty[X, Any](array, index, replaced)(patch(_, index, _, _))

		def patchArrayLikeProperty[X, U >: X :ClassTag :Arbitrary :Shrink :Prettify]
		                          (array :Array[X], index :Int, replace :Int => Int)
		                          (patch :(Array[X], ArrayLike[U], Int) => Array[U]) :Prop =
			forAll { elems :Array[U] =>
				val refArray = RefArray.copyOf(elems)
				val replaced = replace(elems.length)
				(
					patchProperty(array, elems, index, replaced)(patch(array, elems, replaced)) lbl
						"patch: " + elems.contentsString
				) && (
					patchProperty(array, refArray, index, replaced)(patch(array, refArray, replaced)) lbl
						elems.mkString("patch: RefArray[" + localNameOf[U] + "](", ", ", ")")
				)
			}
	}

	private trait PatchArrayProperty extends GenericPatchProperty {
		override def argList = "(Array)"
		def patch[X, Y >: X](array :Array[X], index :Int, elems :Array[Y], replaced :Int) :Array[Y]

		override def apply[X :ClassTag :Arbitrary :Shrink :Prettify]
			              (array :Array[X], index :Int, replaced :Int => Int) :Prop =
			patchArrayProperty[X, X](array, index, replaced)(patch(_, index, _, _)) &&
				patchArrayProperty[X, Any](array, index, replaced)(patch(_, index, _, _))

		def patchArrayProperty[X, U >: X :ClassTag :Arbitrary :Shrink :Prettify]
		                      (array :Array[X], index :Int, replace :Int => Int)
		                      (patch :(Array[X], Array[U], Int) => Array[U]) :Prop =
			forAll { elems :Array[U] =>
				val replaced = replace(elems.length)
				patchProperty(array, elems, index, replaced)(patch(array, elems, replaced)) lbl
					"patch: " + elems.contentsString
			}
	}



/*
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
*/



//	property("subseqOf") = forAll { seq :Seq[Int] =>
//		Prop(seq.filter(_ % 3 == 1) subseqOf seq)
//	}
}






object ArrayObjectExtensionSpec extends ArrayTestingUtils("ArrayObjectExtension") {

	new ArrayProperty("copyOfRange(Array[], Int, Int, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from :Int, until :Int, offset :Int, newLength :Short) =>
				if (offset < 0)
					if (!throws(classOf[IndexOutOfBoundsException])(Array.copyOfRange(array, from, until, offset, newLength))) {
						Console.err.println(s"Array.copyOfRange(${array.contentsString}, $from, $until, $offset, $newLength)")
						Prop.falsified :| "out of range"
					} else
						Array.copyOfRange(array, from, until, offset, newLength).throws[IndexOutOfBoundsException]
				else if (newLength < 0)
					if (!throws(classOf[NegativeArraySizeException])(Array.copyOfRange(array, from, until, offset, newLength))) {
						Console.err.println(s"Array.copyOfRange(${array.contentsString}, $from, $until, $offset, $newLength)")
						Prop.falsified :| "negative array size"
					} else
						Array.copyOfRange(array, from, until, offset, newLength).throws[NegativeArraySizeException]
				else {
					val range = Array.copyOfRange(array, from, until, offset, newLength)
					(((range.getClass :Any) ?= array.getClass) lbl
						s"create ${array.localClassName} is of the same class as the argument ${array.localClassName}"
					) &&((range.length ?= newLength)
						lbl s"returns an array of length $newLength"
					) && {
						val expect = new Array[X](newLength)
						array.view.slice(from, until).copyToArray(expect, offset)
						if (range.toSeq != expect.toSeq)
							Console.err.println(s"Array.copyOfRange(${array.contentsString}, $from, $until, $offset, $newLength) == $range != $expect")
						range.toSeq ?= expect.toSeq
					} lbl s"${array.contentsString}.copyOfRange($from, $until, $offset, $newLength) =? ${range.contentsString}"
				}
			}
	}


	new ArrayProperty("copyOfRange(ArrayLike, Int, Int, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			prop(array, true) && forAll { array :Array[Int] =>
				val refArray = Array.copyAs[Any](array, array.length)
				prop(refArray, false) :| "RefArray[Int]"
			}

		private def prop[X :ClassTag](array :ArrayLike[X], expectSameClass :Boolean) =
			forAll { (from :Int, until :Int, offset :Int, newLength :Short) =>
				if (offset < 0)
					Array.copyOfRange(array, from, until, offset, newLength).throws[IndexOutOfBoundsException]
				else if (newLength < 0)
					Array.copyOfRange(array, from, until, offset, newLength).throws[NegativeArraySizeException]
				else {
					val range = Array.copyOfRange(array, from, until, offset, newLength)
					val p =
						if (expectSameClass)
							((range.getClass :Any) ?= array.getClass) lbl
								s"create ${array.localClassName} is of the same class as the argument ${array.localClassName}"
						else Prop.passed
					p &&((range.length ?= newLength)
						lbl s"returns an array of length $newLength"
					) && {
						val expect = new Array[X](newLength)
						array.view.slice(from, until).copyToArray(expect, offset)
						if (range.toSeq != expect.toSeq)
							Console.err.println(s"Array.copyOfRange(${array.contentsString}, $from, $until, $offset, $newLength) == $range != $expect")
						range.toSeq ?= expect.toSeq
					} lbl s"${array.contentsString}.copyOfRange($from, $until, $offset, $newLength) =? ${range.contentsString}"
				}
			}
	}


	new ArrayProperty("copyOfRanges(Array, Int, Int, Array, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from1 :Int, until1 :Int) =>
				forAll { (array2 :Array[X], from2 :Int, until2 :Int) =>
					ArraySeq.unsafeWrapArray(Array.copyOfRanges(array, from1, until1, array2, from2, until2)) ?=
						(array.view.slice(from1, until1) ++ array2.view.slice(from2, until2) to ArraySeq)
				}
			}
	}

	new ArrayProperty("copyOfRanges(ArrayLike, Int, Int, ArrayLike, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from1 :Int, until1 :Int) =>
				forAll { (array2 :Array[X], from2 :Int, until2 :Int) =>
					val refArray = array.toArray[Any].asInstanceOf[RefArray[X]]
					val iArray   = array2.asInstanceOf[IArray[X]]
					ArraySeq.unsafeWrapArray(Array.copyOfRanges(refArray, from1, until1, iArray, from2, until2)) ?=
						(array.view.slice(from1, until1) ++ array2.view.slice(from2, until2) to ArraySeq)
				}
			}
	}


	new ArrayProperty("copyOfRanges(Array, Int, Int, Array, Int, Int, Array, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from1 :Int, until1 :Int) =>
				forAll { (array2 :Array[X], from2 :Int, until2 :Int) =>
					forAll { (array3 :Array[X], from3 :Int, until3 :Int) =>
						val res = Array.copyOfRanges(array, from1, until1, array2, from2, until2, array3, from3, until3)
						ArraySeq.unsafeWrapArray(res) ?= (
							array.view.slice(from1, until1) ++ array2.view.slice(from2, until2) ++
								array3.slice(from3, until3) to ArraySeq
						)
					}
				}
			}
	}

	new ArrayProperty("copyOfRanges(ArrayLike, Int, Int, ArrayLike, Int, Int)") {
		override def apply[X :ClassTag :Ordering :DefaultValue :Arbitrary :Shrink :Prettify](array :Array[X]) :Prop =
			forAll { (from1 :Int, until1 :Int) =>
				forAll { (array2 :Array[X], from2 :Int, until2 :Int) =>
					forAll { (array3 :Array[X], from3 :Int, until3 :Int) =>
						val refArray  = array.toArray[Any].asInstanceOf[RefArray[X]]
						val iArray2   = array2.asInstanceOf[IArray[X]]
						val refArray3 = array3.toArray[Any].asInstanceOf[IRefArray[X]]
						val res = Array.copyOfRanges(
							refArray, from1, until1, iArray2, from2, until2, refArray3, from3, until3
						)
						ArraySeq.unsafeWrapArray(res) ?= (
							array.view.slice(from1, until1) ++ array2.view.slice(from2, until2) ++
								array3.view.slice(from3, until3) to ArraySeq
						)
					}
				}
			}
	}

	property("generate") = {
		val res = Array.generate(1) { case i if i < 1000 => i * 2 }
		(res.getClass.castParam[Array[Int]] ?= classOf[Array[Int]]) &&
			(res.toList ?= List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))

	}
	property("expand") = {
		val res = Array.expand(2L) { i => Option(i * i).filter(_ < 1000) }
		(res.getClass.castParam[Array[Long]] ?= classOf[Array[Long]]) && (res.toList ?= List(2, 4, 16, 256))
	}
	property("iterateWithIndex") = {
		val res = Array.iterateWithIndex("0", 10)((acc, i) => acc + "," + i)
		(res.getClass.castParam[Array[String]] == classOf[Array[String]]) &&
			(res.toSeq ?= Seq(
				"0", "0,1", "0,1,2", "0,1,2,3", "0,1,2,3,4", "0,1,2,3,4,5", "0,1,2,3,4,5,6", "0,1,2,3,4,5,6,7",
				"0,1,2,3,4,5,6,7,8", "0,1,2,3,4,5,6,7,8,9"
			))
	}
	property("unfold") = {
		val res = Array.unfold(1)(i => Some((i % 10, i * 2)).filter(_._2 < 1000))
		(res.getClass.castParam[Array[Int]] ?= classOf[Array[Int]]) && (res.toSeq ?= Seq(1, 2, 4, 8, 6, 2, 4, 8, 6))
	}

}