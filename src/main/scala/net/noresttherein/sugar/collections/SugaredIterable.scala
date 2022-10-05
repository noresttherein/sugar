package net.noresttherein.sugar.collections

import scala.collection.{IterableOps, SpecificIterableFactory}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.JavaTypes.JIterator




trait SpecificIterableOps[E, +CC[_], +C <: CC[E]] extends SugaredIterableOps[E, CC, C] {
	protected override def fromSpecific(coll :IterableOnce[E]) :C = specificFactory.fromSpecific(coll)
	protected override def newSpecificBuilder :Builder[E, C] = specificFactory.newBuilder
	override def empty :C = specificFactory.empty
	protected def specificFactory :SpecificIterableFactory[E, C]
}




/**
  * @author Marcin Mościcki
  */
trait SugaredIterableOps[+A, +CC[_], +C] extends Any with IterableOps[A, CC, C] {
	def outOfOrder[U](f :A => U) :Unit = foreach(f)
	def pureMap[O](f :A => O) :CC[O] = map(f)
	def pureFlatMap[O](f :A => IterableOnce[O]) :CC[O] = flatMap[O](f)
	def jiterator[I <: JIterator[_]](implicit shape :JavaIteratorShape[A, I]) :I =
		stepper(shape.stepperShape).javaIterator.asInstanceOf[I]
}


trait SugaredIterable[+A] extends Iterable[A] with SugaredIterableOps[A, Iterable, Iterable[A]]
//	   with IterableFactoryDefaults[A, SugaredIterable]
//{
//	override def iterableFactory :IterableFactory[SugaredIterable] = SugaredIterable
//}
//
//
///** $factoryInfo
//  * @define Coll `SugaredIterable`
//  * @define coll sugared iterable
//  */
//@SerialVersionUID(ver)
//object SugaredIterable extends IterableFactory.Delegate[SugaredIterable](PassedArray)
//
//
//
//
//trait SugaredIndexedSeqOps[E, +CC[_], C] extends SugaredIterableOps[E, CC, C] with IndexedSeqOps[E, CC, C]
//
//trait SugaredIndexedSeq[E]
//	extends IndexedSeq[E] with SugaredIterable[E] with SugaredIndexedSeqOps[E, SugaredIndexedSeq, SugaredIndexedSeq[E]]
//	   with IterableFactoryDefaults[E, SugaredIndexedSeq]
//{
//	override def iterableFactory :SeqFactory[SugaredIndexedSeq] = SugaredIndexedSeq
//}
//
//@SerialVersionUID(ver)
//object SugaredIndexedSeq extends SeqFactory.Delegate[SugaredIndexedSeq](PassedArray)

