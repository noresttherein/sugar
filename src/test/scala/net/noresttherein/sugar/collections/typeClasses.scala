package net.noresttherein.sugar.collections

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

import net.noresttherein.sugar.testing.scalacheck.extensions.GenToGenDisjunction
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.util.Buildable

/**
  * @author Marcin Mościcki
  */
object typeClasses {
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

	implicit def iterableOnceGenerator[A :Arbitrary] :Arbitrary[IterableOnce[A]] =
		Arbitrary(Arbitrary.arbitrary[List[A]] /*.map(_.iterator)*/)

}
