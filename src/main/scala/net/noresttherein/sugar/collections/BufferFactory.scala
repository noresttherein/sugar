package net.noresttherein.sugar.collections

import scala.collection.{ClassTagSeqFactory, SeqFactory}
import scala.collection.mutable.{ArrayBuffer, Buffer, Builder, GrowableBuilder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.collections.extensions.BufferExtension




/** An extension of `SeqFactory` with a method allowing to create buffer instances with pre-reserved space
  * for a requested number of elements.
  * @define Coll `Buffer`
  * @define coll buffer
  */
trait BufferFactory[+C[A] <: Buffer[A]] extends SeqFactory[C] {
	/** A new, empty $Coll. Same as `empty`, but slightly more succinct, and puts emphasis on the element type. */
	@inline final def of[E] :C[E] = empty[E]

	/** A new $Coll, with space reserved for `capacity` elements. Works similarly to
	  * [[collection.mutable.Builder Builder]]`.`[[collection.mutable.Builder.sizeHint sizeHint]].
	  * Not all implementations support pre-reserving space.
	  */
	def ofCapacity[E](capacity :Int) :C[E]

	override def from[E](source :IterableOnce[E]) :C[E] = empty[E] ++= source

	override def newBuilder[E] :Builder[E, C[E]] = new GrowableBuilder[E, C[E]](empty)
}


/** An extension of `IterableFactory` with a method allowing to create buffer instances with pre-reserved space
  * for a requested number of elements.
  * @tparam C the buffer type constructor.
  * @tparam Ev evidence type class required for the element type of created buffers.
  * @define Coll `Buffer`
  * @define coll buffer
  */
trait ClassTagBufferFactory[+C[A] <: Buffer[A]] extends ClassTagSeqFactory[C] {
	/** A new, empty $Coll. Same as `empty`, but slightly more succinct, and puts emphasis on the element type. */
	@inline final def of[E :ClassTag] :C[E] = empty[E]

	/** A new $Coll, with space reserved for `capacity` elements. Works similarly to
	  * [[collection.mutable.Builder Builder]]`.`[[collection.mutable.Builder.sizeHint sizeHint]].
	  * Not all implementations support pre-reserving space.
	  */
	def ofCapacity[E :ClassTag](capacity :Int) :C[E]

	override def from[E :ClassTag](source :IterableOnce[E]) :C[E] = empty[E] ++= source

	override def newBuilder[E :ClassTag] :Builder[E, C[E]] = new GrowableBuilder[E, C[E]](empty)
}


@SerialVersionUID(Ver)
case object ArrayBufferFactory extends BufferFactory[ArrayBuffer] {
	override def ofCapacity[E](capacity :Int) :ArrayBuffer[E] = new ArrayBuffer(capacity)
	override def empty[A] :ArrayBuffer[A] = new ArrayBuffer
}

@SerialVersionUID(Ver)
private class BufferFactoryAdapter[C[A] <: Buffer[A]](factory :SeqFactory[C])
	extends SeqFactory.Delegate[C](factory) with BufferFactory[C]
{
	override def ofCapacity[E](capacity :Int) :C[E] = empty[E] trySizeHint capacity
	override def toString = factory.toString
}
