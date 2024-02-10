package net.noresttherein.sugar.arrays

import scala.collection.ClassTagIterableFactory

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.collections.{ArrayLikeSlice, ArraySlice, ArraySliceOps, ArraySliceSeqOps, IArraySlice, MatrixBuffer}
import net.noresttherein.sugar.vars.Maybe
import net.noresttherein.sugar.vars.Maybe.{No, Yes}


/**
  * @author Marcin Mościcki
  */
@SerialVersionUID(Ver)
private[sugar] object ProperArray extends ClassTagIterableFactory.Delegate[ProperArray](ArrayFactory) {

	@SerialVersionUID(Ver)
	object Wrapped {
		def apply[E](array :ProperArray[E]) :collection.IndexedSeq[E] = Sliced(array, 0, array.length)

		def unapply[E](items :IterableOnce[E]) :Maybe[ProperArray[E]] = items match {
			case seq :IArraySlice[E] if seq.length == seq.unsafeArray.length =>
				Yes(seq.unsafeArray.asInstanceOf[ProperArray[E]])
			case seq :ArraySlice[E] if seq.length == seq.unsafeArray.length =>
				Yes(seq.unsafeArray.asInstanceOf[ProperArray[E]])
			case seq :ArraySliceOps[E @unchecked, collection.Seq @unchecked, collection.Seq[E] @unchecked]
				if seq.elementType != classOf[Any] && seq.size == seq.unsafeArray.length
			=>
				Yes(seq.unsafeArray.castFrom[Array[_], ProperArray[E]])
			case seq :MatrixBuffer[E]
				if seq.dim == 1 && seq.data1.getClass != classOf[Array[Any]] &&
					seq.length == seq.data1.length && seq.startOffset == 0 &&
					seq.data1.getClass.getComponentType != classOf[Any]
			=>
				Yes(seq.data1)
			case _ => No
		}

		@SerialVersionUID(Ver)
		object Sliced {
			def apply[E](array :ProperArray[E], from :Int, until :Int) :collection.IndexedSeq[E] =
				ArrayLikeSlice.slice(array, from, until)
			
			def unapply[E](items :IterableOnce[E]) :Maybe[(ProperArray[E], Int, Int)] = items match {
				case seq :IArraySlice[E] =>
					val offset = seq.startIndex
					Yes((seq.unsafeArray.asInstanceOf[ProperArray[E]], offset, offset + seq.length))
				case seq :ArraySlice[E] =>
					val offset = seq.startIndex
					Yes((seq.unsafeArray.asInstanceOf[ProperArray[E]], offset, offset + seq.length))
				case seq :ArraySliceSeqOps[E @unchecked, collection.Seq @unchecked, collection.Seq[E] @unchecked]
					if seq.elementType != classOf[Any]
				=>
					val offset = seq.startIndex
					Yes((seq.unsafeArray.castFrom[Array[_], ProperArray[E]], offset, offset + seq.length))
				case seq :MatrixBuffer[E]
					if seq.dim == 1 && seq.data1.getClass != classOf[Array[Any]] &&
						seq.startOffset + seq.length <= seq.data1.length && seq.startOffset == 0
				=>
					val offset = seq.startOffset
					Yes((seq.data1, offset, offset + seq.length))
				case _ => No
			}
		}
	}
}
