package net.noresttherein.sugar.testing.scalacheck

import scala.annotation.nowarn
import scala.collection.View
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.arrays.{ArrayLike, IArray, IArrayLike, IRefArray, MutableArray, RefArray, RefArrayLike}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.{IRefArraySlice, RefArraySlice}
import net.noresttherein.sugar.testing.scalacheck.extensions.GenToGenDisjunction
import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen, Shrink}




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


	implicit def arbitraryArrayLike[T :Arbitrary :ClassTag] :Arbitrary[ArrayLike[T]] =
		arbitraryIArrayLike[T].upcastParam[ArrayLike[T]]

	implicit def arbitraryIArrayLike[T :Arbitrary :ClassTag] :Arbitrary[IArrayLike[T]] = Arbitrary(
		Gen.oneOf(arbitraryIArray[T].arbitrary, arbitraryIRefArray[T].arbitrary)
	)
	implicit def arbitraryIArray[T :Arbitrary :ClassTag] :Arbitrary[IArray[T]] = Arbitrary(
		Arbitrary.arbitrary[Array[T]].castParam[IArray[T]]
	)
	implicit def arbitraryIRefArray[T :Arbitrary] :Arbitrary[IRefArray[T]] = Arbitrary(
		Arbitrary.arbitrary[ArrayBuffer[T]].map(_ to IRefArray)
	)
	implicit def arbitraryRefArray[T :Arbitrary] :Arbitrary[RefArray[T]] = Arbitrary(
		Arbitrary.arbitrary[ArrayBuffer[T]].map(_ to RefArray)
	)
	implicit def arbitraryRefArrayLike[T :Arbitrary] :Arbitrary[RefArrayLike[T]] =
		arbitraryRefArray[T].upcastParam[RefArrayLike[T]]

	implicit def arbitraryMutableArray[T :Arbitrary :ClassTag] :Arbitrary[MutableArray[T]] = Arbitrary(
		Gen.oneOf(
			Arbitrary.arbitrary[Array[T]].upcastParam[MutableArray[T]],
			arbitraryRefArray[T].arbitrary.upcastParam[MutableArray[T]])
	)


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

	implicit def buildableView[T] :Buildable[T, View[T]] = new Buildable[T, View[T]] {
		override def builder = List.newBuilder[T].mapResult { list => View.fromIteratorProvider(() => list.iterator) }
	}


	implicit def arbitraryIterableOnce[A :Arbitrary] :Arbitrary[IterableOnce[A]] =
		Arbitrary(Arbitrary.arbitrary[List[A]] /*.map(_.iterator)*/)

}
