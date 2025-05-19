package net.noresttherein.sugar.arrays


import scala.collection.{ArrayOps, BufferedIterator}
import scala.collection.immutable.{AbstractSeq, ArraySeq}
import scala.reflect.ClassTag

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Prop}
import net.noresttherein.sugar.collections.CuboidSliceFactory
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.testing.scalacheck.extensions.PropExtension




abstract class CuboidIteratorProps[I[+X] <: BufferedIterator[X]]
                                  (name :String, override val factory :CuboidSliceFactory[Array, I])
	extends IndexedIteratorProps[Array3, I](name, factory)
{
	import net.noresttherein.sugar.testing.scalacheck.noShrinking

	abstract class CuboidProperty(name :String) extends IteratorProperty(name) {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array3[X]) :Prop = {
			val length3 = source.length
			val length2 = if (length3 == 0) 0 else source(0).length
			val length1 = if (length2 == 0) 0 else source(0)(0).length
			apply(source, length3, length2, length1)
		}
		def apply[X :ClassTag :Ordering :Arbitrary](source :Array3[X], length3 :Int, length2 :Int, length1 :Int) :Prop
	}

	val MaxDim = 10
	implicit def arbitraryCuboid[X :ClassTag :Arbitrary] :Arbitrary[Array3[X]] = Arbitrary {
		for {
			dim3 <- Gen.choose(0, MaxDim)
			dim2 <- Gen.choose(0, MaxDim)
			dim1 <- Gen.choose(0, MaxDim)
			gen1  = Gen.buildableOfN[Array[X], X](dim1, Arbitrary.arbitrary[X])
			gen2  = Gen.buildableOfN[Array2[X], Array[X]](dim2, gen1)
			a <- Gen.buildableOfN[Array3[X], Array2[X]](dim3, gen2)
		} yield a
	}
	protected override def lengthOf[X](source :Array3[X]) :Int =
		if (source.length == 0) 0
		else if (source(0).length == 0) 0
		else source.length * source(0).length * source(0)(0).length

	protected override def seq[X](source :Array3[X]) :Seq[X] = ArraySeq.unsafeWrapArray(source).flatten.flatten
	protected override def iterator[X](source :Array3[X]) :Iterator[X] = new ArrayOps(source).iterator.flatten.flatten
	protected def ops[X](array :Array[X]) :ArrayOps[X] = new ArrayOps(array)

	private def clone[X](a :Array3[X]) :Array3[X] = {
		val res = a.clone()
		var i = a.length - 1
		while (i > 0) {
			i -= 1
			val a2 = a(i).clone()
			res(i) = a2
			var j = a2.length
			while (j > 0) {
				j -= 1
				a2(j) = a2(j).clone()
			}
		}
		res
	}

	protected def dimensions[X](source :Array3[X]) :String = {
		val length3 = source.length
		val length2 = if (length3 == 0) 0 else source(0).length
		val length1 = if (length2 == 0) 0 else source(0)(0).length
		s"Dimensions = $length3 x $length2 x $length1"
	}

	protected override def contents[X](a3 :Array3[X]) :String =
		if (a3.length == 0)
			a3.className + "()"
		else
			a3.iterator.map(
				a2 => a2.iterator.map(
					a1 => a1.mkString("        " + a1.className + "(", ", ", ")")
				).mkString("    " + a2.className + "(\n", ",\n", "\n    )")
			).mkString(a3.className + "(\n", ",\n", "\n)")

	protected override def forAllInputs(prop :IteratorProperty) :Prop =
		forAll { a :Array3[Byte]    => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Short]   => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Char]    => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Int]     => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Long]    => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Float]   => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Double]  => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Boolean] => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[Unit]    => prop(clone(a)) lbl contents(a) lbl dimensions(a) } &&
		forAll { a :Array3[String]  => prop(clone(a)) lbl contents(a) lbl dimensions(a) }


	protected def expectSlice[X](source :Array3[X], from3 :Int, from2 :Int, from1 :Int,
	                             until3 :Int, until2 :Int, until1 :Int) :Seq[X] =
		if (from3 < 0)
			expectSlice(source, 0, 0, 0, until3, until2, until1)
		else if (until3 < 0 || source.length == 0 || source(0).length == 0 || source(0)(0).length == 0)
			Seq.empty
		else {
			val len3      = source.length
			val len2      = source(0).length
			val len1      = source(0)(0).length
			val start3    = clip(from3, len3)
			val end3      = clip(until3, len3)
			val start2    = if (from3 < 0 | from3 >= len3) 0 else clip(from2, len2)
			val end2      = if (until3 < 0 | until3 >= len3) 0 else clip(until2, len2)
			val start1    =
				if (from2 < 0 | from1 < 0) -1
				else if (from3 >= len3 | from2 >= len2) 0
				else if (from1 >= len1) len1
				else from1
			val end1      =
				if (until1 < 0 | until2 < 0 | until3 < 0) -1
				else if (until3 >= len3 | until2 >= len2) 0
				else if (until1 >= len1) len1
				else until1
			expectSlice(source, (start3 * len2 + start2) * len1 + start1, (end3 * len2 + end2) * len1 + end1)
		}
	//We can't just easily delegate to expectSlice without checking for overflows and underflows, at which point
	// we did most of the work anyway.
	protected def expectApply[X](source :Array3[X], from3 :Int, from2 :Int, from1 :Int, size :Int) :Seq[X] = {
		if (size <= 0 || source.length == 0 || source(0).length == 0 || source(0)(0).length == 0)
			Seq.empty
		else if (from3 < 0)
			expectApply(source, direction, size)
		else if (from2 < 0 | from2 == 0 & from1 < 0)
			expectApply(source, math.min(from3, source.length) - 1, source(0).length - 1, source(0)(0).length - 1, size)
		else if (from3 >= source.length && from2 > 0)
			expectApply(source, source.length, 0, from1, size)
		else if (from1 < 0)
			expectApply(source, math.min(from3, source.length - 1), math.min(from2, source(0).length) - 1, source(0)(0).length - 1, size)
		else if (from3 > source.length)
			expectApply(source, source.length, 0, 0, size)
		else if (from2 >= source(0).length)
			expectApply(source, from3 + 1, 0, 0, size)
		else if (from1 >= source(0)(0).length)
			expectApply(source, from3, from2 + 1, 0, size)
		else {
			val length2  = source(0).length
			val length1  = source(0)(0).length
			expectApply(source, (from3 * length2 + from2) * length1 + from1, size)
		}
	}

	protected def direction = 0 //0 for forward iterators and -1 for reverse iterators.

	new IteratorProperty("slice(array, from3, from2, from1, until3, until2, until1)") {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array3[X]) :Prop =
			forAll { (from3 :Int, from2 :Int, from1 :Int, until3 :Int, until2 :Int, until1 :Int) =>
				val expect = expectSlice(source, from3, from2, from1, until3, until2, until1)
				apply(expect, factory.slice(source, from3, from2, from1, until3, until2, until1)) lbl
					s"$name.slice(${contents(source)}, $from3, $from2, $from1, $until3, $until2, $until1) == " +
						factory.slice(
							source, from3, from2, from1, until3, until2, until1
						).mkString("Iterator(", ", ", ")") + "\n!= " + expect
			}
	}
	new CuboidProperty("slice(length2, length1, array, from, until)") {
		override def apply[X :ClassTag :Ordering :Arbitrary]
		                  (source :Array3[X], length3 :Int, length2 :Int, length1 :Int) :Prop =
			forAll { (from :Int, until :Int) =>
				val expect = expectSlice(source, from, until)
				apply(expect, factory.slice(length2, length1, source, from, until)) lbl
					s"$name.slice($length2, $length1, ${contents(source)}, $from, $until) == " +
						factory.slice(length2, length1, source, from, until).mkString("Iterator(", ", ", ")") +
						"\n!= " + expect
			}
	}

	new IteratorProperty("apply(array, from3, from2, from1, size)")  {
		override def apply[X :ClassTag :Ordering :Arbitrary](source :Array3[X]) :Prop =
			forAll { (from3 :Int, from2 :Int, from1 :Int, size :Int) =>
				val expect = expectApply(source, from3, from2, from1, size)
				apply(expect, factory(source, from3, from2, from1, size)) lbl
					s"$name(${contents(source)}, $from3, $from2, $from1, $size) == " +
						factory(source, from3, from2, from1, size).mkString("Iterator(", ", ", ")") +
						"\n!= " + expect
			}
	}
	new CuboidProperty("apply(length2, length1, array, from, size)") {
		override def apply[X :ClassTag :Ordering :Arbitrary]
		                  (source :Array3[X], length3 :Int, length2 :Int, length1 :Int) :Prop =
			forAll { (from :Int, size :Int) =>
				val expect = expectApply(source, from, size)
				apply(expect, factory(length2, length1, source, from, size)) lbl
					s"$name($length2, $length1, ${contents(source)}, $from, $size) == " +
						factory(length2, length1, source, from, size).mkString("Iterator(", ", ", ")") +
						"\n!= " + expect
			}
	}
}



object CuboidIteratorSpec extends CuboidIteratorProps[BufferedIterator]("CuboidIterator", CuboidIterator)

object ReverseCuboidIteratorSpec
	extends CuboidIteratorProps[BufferedIterator]("ReverseCuboidIterator", ReverseCuboidIterator)
		with ReverseIndexedIteratorProps[Array3, BufferedIterator]
{
	protected override def direction = -1
}




abstract class CyclicCuboidIteratorProps[I[+X] <: BufferedIterator[X]]
                                        (name :String, factory :CuboidSliceFactory[Array, I])
	extends CuboidIteratorProps[I](name, factory)
{
	protected override def expectSlice[X](source :Array3[X], from3 :Int, from2 :Int, from1 :Int,
	                                      until3 :Int, until2 :Int, until1 :Int) :Seq[X] =
		if (source.length == 0 || source(0).length == 0 || source(0)(0).length == 0)
			Seq.empty
		else {
			val length3 = source.length
			val length2 = source(0).length
			val length1 = source(0)(0).length
			def delta3(i3 :Int, i2 :Int, i1 :Int) =
				if (i2 < 0 | i2 == 0 & i1 < 0) -1
				else if (i2 >= length2 & i1 >= 0 || i2 == length2 - 1 && i1 >= length1) 1
				else 0
			def index3(i3 :Int, i2 :Int, i1 :Int) =
				mod(i3.toLong + delta3(i3, i2, i1), length3)
			def index2(i2 :Int, i1 :Int) =
				if (i1 < 0 | i2 < 0)
					if (i2 <= 0) length2 - 1 else math.min(i2, length2) - 1
				else if (i2 >= length2 || i2 == length2 - 1 & i1 >= length1)
					0
				else if (i1 >= length1)
					i2 + 1
				else
					i2
			def index1(i2 :Int, i1 :Int) =
				if (i2 < 0 | i1 < 0) length1 - 1
				else if (i2 >= length2 | i1 >= length1) 0
				else i1
			val start3 = index3(from3, from2, from1)
			val start2 = index2(from2, from1)
			val start1 = index1(from2, from1)
			val end3   = index3(until3, until2, until1)
			val end2   = index2(until2, until1)
			val end1   = index1(until2, until1)
			val from   = ((from3.toLong + delta3(from3, from2, from1)) * length2 + start2) * length1 + start1
			val until  = ((until3.toLong + delta3(until3, until2, until1)) * length2 + end2) * length1 + end1
			val start  = (start3 * length2 + start2) * length1 + start1
			val end    = (end3 * length2 + end2) * length1 + end1
			if (from == until)
				Seq.empty
			else
				expectSlice(source, start, end + length3 * length2 * length1)
		}

	protected override def expectApply[X](source :Array3[X], from3 :Int, from2 :Int, from1 :Int, size :Int) :Seq[X] =
		if (size <= 0 || source.length == 0 || source(0).length == 0 || source(0)(0).length == 0)
			Seq.empty
		else {
			val length3 = source.length
			val length2 = source(0).length
			val length1 = source(0)(0).length
			val length  = length3 * length2 * length1
			val mod3    = if (from3 < 0) (length + from3 % length) % length3 else from3 % length
			val delta3  =
				if (from2 < 0 | from2 == 0 & from1 < 0) -1
//				else if (from2 >= length2 & from1 >= 0 || from2 == length2 - 1 && from1 >= length1) 1
				else 0
			val start3  =
				if (delta3 < 0 & mod3 == 0) length3 - 1
				else (mod3 + delta3) % length3
			val start2  =
				if (delta3 < 0) length2 - 1
				else if (from1 < 0) math.min(from2, length2) - 1
				else math.min(length2, from2)
			val start1 =
				if (from2 < 0 | from1 < 0) length1 - 1
				else if (from2 >= length2) 0
				else math.min(from1, length1)
			val from    = (start3 * length2 + start2) * length1 + start1
			expectApply(source, from % length, size)
		}
}


object CyclicCuboidIteratorSpec
	extends CyclicCuboidIteratorProps[BufferedIterator]("CyclicCuboidIterator", CyclicCuboidIterator)
		with CyclicIndexedIteratorProps[Array3, BufferedIterator]


object ReverseCyclicCuboidIteratorSpec
	extends CyclicCuboidIteratorProps[BufferedIterator]("ReverseCyclicCuboidIterator", ReverseCyclicCuboidIterator)
		with ReverseCyclicIndexedIteratorProps[Array3, BufferedIterator]
