package net.noresttherein.sugar.arrays

import scala.collection.{ArrayOps, BufferedIterator}
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}

import net.noresttherein.sugar.collections.MatrixSliceFactory
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension




abstract class MatrixIteratorProps[I[+X] <: BufferedIterator[X]]
                                  (name :String, override val factory :MatrixSliceFactory[Array, I])
	extends IndexedIteratorProps[Array2, I](name, factory)
{
	val MaxDim = 10
	implicit def arbitraryMatrix[X :ClassTag :Arbitrary] :Arbitrary[Array2[X]] = Arbitrary {
		for {
			dim2 <- Gen.choose(0, MaxDim)
			dim1 <- Gen.choose(0, MaxDim)
			a <- Gen.buildableOfN[Array2[X], Array[X]](dim2, Gen.buildableOfN[Array[X], X](dim1, Arbitrary.arbitrary[X]))
		} yield a
	}
	protected override def lengthOf[X](source :Array2[X]) :Int =
		if (source.length == 0) 0 else source.length * source(0).length

	protected override def seq[X](source :Array2[X]) :Seq[X] = ArraySeq.unsafeWrapArray(source).flatten
	protected override def iterator[X](source :Array2[X]) :Iterator[X] = new ArrayOps(source).iterator.flatten
	protected def ops[X](array :Array[X]) :ArrayOps[X] = new ArrayOps(array)

	private def clone[X](a :Array2[X]) :Array2[X] = {
		val res = a.clone()
		var i = a.length
		while (i > 0) {
			i -= 1
			res(i) = a(i).clone()
		}
		res
	}

	protected override def contents[X](a2 :Array2[X]) :String =
		if (a2.length == 0)
			a2.className + "()"
		else
			a2.iterator.map(
				a1 => a1.mkString("    " + a1.className + "(", ", ", ")")
			).mkString(a2.className + "(\n", ",\n", "\n)")

	protected override def forAllInputs(prop :IteratorProperty) :Prop =
		forAll { a :Array2[Byte]    => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Short]   => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Char]    => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Int]     => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Long]    => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Float]   => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Double]  => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Boolean] => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[Unit]    => prop(clone(a)) lbl contents(a) } &&
		forAll { a :Array2[String]  => prop(clone(a)) lbl contents(a) }


	protected def expectSlice[X](source :Array2[X], from2 :Int, from1 :Int, until2 :Int, until1 :Int) :Seq[X] =
		if (from2 < 0)
			expectSlice(source, 0, 0, until2, until1)
		else if (until2 < 0 || source.length == 0 || source(0).length == 0)
			Seq.empty
		else {
			val start2    = clip(from2, source.length)
			val end2      = clip(until2, source.length)
			val len1      = source(0).length
			val start1    =
				if (from1 < 0) -1
				else if (from2 >= source.length) 0
				else if (from1 >= len1) len1
				else from1
			val end1      =
				if (until1 < 0) -1
				else if (until2 >= source.length) 0
				else if (until1 > len1) len1
				else until1
			expectSlice(source, start2 * len1 + start1, end2 * len1 + end1)
		}
	//We can't just easily delegate to expectSlice without checking for overflows and underflows, at which point
	// we did most of the work anyway.
	protected def expectApply[X](source :Array2[X], from2 :Int, from1 :Int, size :Int) :Seq[X] =
		if (size <= 0 || source.length == 0 || source(0).length == 0)
			Seq.empty
		else if (from2 < 0)
			expectApply(source, direction, size)
		else if (from1 < 0)
			expectApply(source, math.min(from2, source.length) - 1, source(0).length - 1, size)
		else {
			val len    = source(0).length
			val start2 = clip(from2, source.length)
			val start1 =
				if (from2 >= source.length) 0
				else clip(from1, len)
			expectApply(source, start2 * len + start1, size)
		}
	protected def direction = 0 //0 for forward iterators and -1 for reverse iterators.

	new IteratorProperty("slice(array, from2, from1, until2, until1)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array2[X]) :Prop =
			forAll { (from2 :Int, from1 :Int, until2 :Int, until1 :Int) =>
				val expect = expectSlice(source, from2, from1, until2, until1)
				apply(expect, factory.slice(source, from2, from1, until2, until1)) lbl
					s"$name.slice(${contents(source)}, $from2, $from1, $until2, $until1) == " +
						factory.slice(source, from2, from1, until2, until1).mkString("Iterator(", ", ", ")") +
						"\n!= " + expect
			}
	}
	new IteratorProperty("slice(length, array, from, until)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array2[X]) :Prop =
			forAll { (from :Int, until :Int) =>
				val len  = if (source.length == 0) 0 else source(0).length
				val expect = expectSlice(source, from, until)
				apply(expect, factory.slice(len, source, from, until)) lbl
					s"$name.slice($len, ${contents(source)}, $from, $until) == " +
						factory.slice(len, source, from, until).mkString("Iterator(", ", ", ")") + "\n!= " + expect
			}
	}

	new IteratorProperty("apply(array, from2, from1, size)")  {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array2[X]) :Prop =
			forAll { (from2 :Int, from1 :Int, size :Int) =>
				val expect = expectApply(source, from2, from1, size)
				apply(expect, factory(source, from2, from1, size)) lbl
					s"$name(${contents(source)}, $from2, $from1, $size) == " +
						factory(source, from2, from1, size).mkString("Iterator(", ", ", ")") + "\n!= " + expect
			}
	}
	new IteratorProperty("apply(length, array, from, size)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array2[X]) :Prop =
			forAll { (from :Int, size :Int) =>
				val len  = if (source.length == 0) 0 else source(0).length
				val expect = expectApply(source, from, size)
				apply(expect, factory(len, source, from, size)) lbl
					s"$name($len, ${contents(source)}, $from, $size) == " +
						factory(len, source, from, size).mkString("Iterator(", ", ", ")") + "\n!= " + expect
			}
	}
}



object MatrixIteratorSpec extends MatrixIteratorProps[BufferedIterator]("MatrixIterator", MatrixIterator)


object ReverseMatrixIteratorSpec
	extends MatrixIteratorProps[BufferedIterator]("ReverseMatrixIterator", ReverseMatrixIterator)
		with ReverseIndexedIteratorProps[Array2, BufferedIterator]
{
	protected override def direction = -1
}




abstract class CyclicMatrixIteratorProps[I[+X] <: BufferedIterator[X]]
                                        (name :String, factory :MatrixSliceFactory[Array, I])
	extends MatrixIteratorProps[I](name, factory)
{
	protected override def expectSlice[X](source :Array2[X], from2 :Int, from1 :Int, until2 :Int, until1 :Int) :Seq[X] =
		if (source.length == 0 || source(0).length == 0)
			Seq.empty
		else {
			val length2 = source.length
			val length1 = source(0).length
			val adjustedFrom2  = if (from1 >= length1) from2.toLong + 1 else from2.toLong
			val adjustedUntil2 = if (until1 >= length1) until2.toLong + 1 else until2.toLong
			val clippedFrom1   = clip(from1, length1)// % length1
			val clippedUntil1  = clip(until1, length1)// % length1
			if (length1 == 0 || adjustedFrom2 == adjustedUntil2 && clippedFrom1 % length1 == clippedUntil1 % length1)
				Seq.empty
			else {
				val modFrom2       = if (from2 < 0) length2 + from2 % length2 else from2 % length2
				val modUntil2      = if (until2 < 0) length2 + until2 % length2 else until2 % length2
				val from   = modFrom2 * length1 + clippedFrom1
				val until  = modUntil2 * length1 + clippedUntil1
				if (from == until)
					expectSlice(source, from, from + length2 * length1)
				else
					expectSlice(source, from, until)
			}
		}
	protected override def expectApply[X](source :Array2[X], from2 :Int, from1 :Int, size :Int) :Seq[X] =
		if (size <= 0 || source.length == 0 || source(0).length == 0)
			Seq.empty
		else {
			val length1 = source(0).length
			val length  = length1 * source.length
			val from    = from2.toLong * length1 + math.min(length1, math.max(0, from1))
			expectApply(source, (from % length).toInt, size)
		}
}


object CyclicMatrixIteratorSpec
	extends CyclicMatrixIteratorProps[BufferedIterator]("CyclicMatrixIterator", CyclicMatrixIterator)
		with CyclicIndexedIteratorProps[Array2, BufferedIterator]


object ReverseCyclicMatrixIteratorSpec
	extends CyclicMatrixIteratorProps[BufferedIterator]("ReverseCyclicMatrixIterator", ReverseCyclicMatrixIterator)
		with ReverseCyclicIndexedIteratorProps[Array2, BufferedIterator]
