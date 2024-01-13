package net.noresttherein.sugar.testing

import scala.annotation.nowarn

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.util.Buildable




package object scalacheck {

	@nowarn implicit def noShrinking[T] :Shrink[T] = Shrink { (x :T) => Stream.empty }

	@inline def collectionOf[C[_]] :CollectionGenFactory[C] = new CollectionGenFactory[C] {}

	sealed trait CollectionGenFactory[C[_]] extends Any {
		@inline final def apply[T](gen :Gen[T])(implicit buildable :Buildable[T, C[T]], isIterable :C[T] => Iterable[T])
				:Gen[C[T]] =
			Gen.containerOf[C, T](gen)
	}

	@inline def buildable[C] :BuildableGenFactory[C] = new BuildableGenFactory[C] {}

	sealed trait BuildableGenFactory[C] extends Any {
		@inline final def apply[T]()(implicit buildable :Buildable[T, C], gen :Arbitrary[T], isIterable :C => Iterable[T])
				:Gen[C] =
			Gen.buildableOf[C, T](gen.arbitrary)
	}

	@inline implicit def BuildableGenFactoryToGen[C, T](self :BuildableGenFactory[C])
	                                                   (implicit buildable :Buildable[T, C], gen :Arbitrary[T],
	                                                    isIterable :C => Iterable[T]) :Gen[C] =
		self()
}