package net.noresttherein.sugar.collections

import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.testing.scalacheck.extensions.GenToGenDisjunction
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.util.Buildable




object typeClasses {
	@nowarn("cat=deprecation")
	implicit val shrinkAlphaNumChar :Shrink[Char] = Shrink { c =>
		def shrink(x: Char): Stream[Char] =
			if (x == 0 | x == 'a')
				Stream(x)
			else if (x >= 'a' & x <= 'z') {
				val c = ((x - 'a') / 2 + 'a').toChar
				c #:: shrink(c)
			} else if (x >= 'A' & x <= 'Z')
				x.toLower #:: shrink(x.toLower)
			else if (x >= '0' & x <= '9') {
//				val c = ((x - '0') / 2 + '0').toChar
				val c = (x - '0' + 'a').toChar
				c #:: shrink(c)
			} else
				Shrink.shrinkIntegral[Char].shrink(x)
		if (c == 0 | c == 'a') Stream.empty else shrink(c)
	}
	implicit val shrinkAlphaNumString :Shrink[String] = Shrink { s =>
		Shrink.shrinkContainer[List, Char].shrink(s.toList).map(_.mkString)
	}

	implicit val arbitraryAny :Arbitrary[Any] = Arbitrary {
		Gen.alphaStr || Arbitrary.arbitrary[Int] || Arbitrary.arbitrary[Long] || Arbitrary.arbitrary[Double]
	}

	implicit def buildableRefArraySliceSeq[T] :Buildable[T, RefArraySlice[T]] = new Buildable[T, RefArraySlice[T]] {
		override def builder :Builder[T, RefArraySlice[T]] =
			new Builder[T, RefArraySlice[T]] {
				val underlying = RefArraySlice.newBuilder[T] += null.asInstanceOf[T]
				override def addOne(elem :T) = { underlying += elem; this }
				override def clear() :Unit = underlying.clear()
				override def result() = {
					val arr = (underlying += null.asInstanceOf[T]).result()
					arr.slice(1, arr.length - 1)
				}
			}
	}
	implicit def buildableIRefArraySliceSeq[T] :Buildable[T, IRefArraySlice[T]] =
		new Buildable[T, IRefArraySlice[T]] {
			override def builder :Builder[T, IRefArraySlice[T]] =
				new Builder[T, IRefArraySlice[T]] {
					val underlying = IRefArraySlice.newBuilder[T] += null.asInstanceOf[T]
					override def addOne(elem :T) = { underlying += elem; this }
					override def clear() :Unit = underlying.clear()
					override def result() = {
						val arr = (underlying += null.asInstanceOf[T]).result()
						arr.slice(1, arr.length - 1)
					}
				}
		}


	implicit def buildableArraySeq[T :ClassTag] :Buildable[T, ArraySeq[T]] = new Buildable[T, ArraySeq[T]] {
		override def builder :Builder[T, ArraySeq[T]] = ArraySeq.newBuilder[T]
	}

	implicit def buildableVector[T] :Buildable[T, Vector[T]] = new Buildable[T, Vector[T]] {
		override def builder = Vector.newBuilder[T]
	}


	implicit def arbitraryIterableOnce[A :Arbitrary] :Arbitrary[IterableOnce[A]] =
		Arbitrary(Arbitrary.arbitrary[List[A]] /*.map(_.iterator)*/)

}
