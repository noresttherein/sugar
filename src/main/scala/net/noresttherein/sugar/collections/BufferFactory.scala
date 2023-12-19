package net.noresttherein.sugar.collections

import scala.collection.SeqFactory
import scala.collection.mutable.{Buffer, Builder, GrowableBuilder}




/**
  * @author Marcin Mościcki
  */
trait BufferFactory[+C[A] <: Buffer[A] with collection.SeqOps[A, Buffer, Buffer[A]]] extends SeqFactory[C] {
	/** A new, empty buffer. Same as `empty`, but slightly more succinct, and puts emphasis on the element type. */
	@inline final def of[E] :C[E] = empty[E]

	/** A new buffer, with space reserved for `capacity` elements. Works similarly to
	  * [[collection.mutable.Builder Builder]]`.`[[collection.mutable.Builder.sizeHint sizeHint]].
	  */
	def ofCapacity[E](capacity :Int) :C[E]

	override def from[E](source :IterableOnce[E]) :C[E] = empty[E] ++= source

	override def newBuilder[E] :Builder[E, C[E]] = new GrowableBuilder[E, C[E]](empty)
}
