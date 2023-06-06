package net.noresttherein.sugar.collections

import scala.collection.{IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqFactory}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.mutable.Builder

/**
  * @author Marcin Mościcki
  */
sealed abstract class HyperCube[+E]
	extends AbstractSeq[E] with IndexedSeq[E] with IndexedSeqOps[E, HyperCube, HyperCube[E]]
	   with StrictOptimizedSeqOps[E, HyperCube, HyperCube[E]] with IterableFactoryDefaults[E, HyperCube]
	   with SugaredIterable[E] with SugaredIterableOps[E, HyperCube, HyperCube[E]] with DefaultSerializable
{
	override def iterableFactory :SeqFactory[HyperCube] = HyperCube
}



object HyperCube extends StrictOptimizedSeqFactory[HyperCube] {

	override def from[A](source :IterableOnce[A]) :HyperCube[A] = ???

	override def empty[A] :HyperCube[A] = HyperCube0

	override def newBuilder[A] :Builder[A, HyperCube[A]] = ???

	private final class HyperCube2[+E]()

	private final class HyperCube1[+E](array :Array[E])
		extends HyperCube[E] with AbstractArraySlice[E]
	{
		private[collections] override def unsafeArray :Array[_] = array
		private[collections] override def startIndex :Int = 0

		def length = array.length
		override def apply(i :Int) :E = array(i)
	}

	private object HyperCube0 extends HyperCube[Nothing] {
		override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException("HyperCube.empty(" + i + ")")
		override def length :Int = 0
	}
}
