package net.noresttherein.sugar.collections


import java.util.Spliterator

import scala.annotation.{nowarn, tailrec}
import scala.collection.{Factory, IndexedSeqView, IterableFactory, IterableOnceOps, SeqView, Stepper, StepperShape, View, WithFilter, mutable}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.View.Elems
import scala.collection.immutable.{IndexedSeqOps, StringView, WrappedString}
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.ClassTag

import net.noresttherein.sugar.JavaTypes.{JDouble, JInt, JLong, JStringBuilder}
import net.noresttherein.sugar.arrays.{ArrayCompanionExtension, ArrayExtension, ArrayFactory, ArrayIterator, ArrayLike, ArrayLikeExtension, ArrayLikeSpecOps, IArray, IArrayExtension, IArrayLike, IArrayLikeExtension, IRefArray, MutableArray, MutableArrayExtension, RefArray, RefArrayExtension, RefArrayLike, RefArrayLikeExtension, ReverseArrayLikeIterator}
import net.noresttherein.sugar.casting.castTypeParamMethods
import net.noresttherein.sugar.collections.IndexedIterable.ApplyPreferred
import net.noresttherein.sugar.collections.LikeIndexedSeq.LikeIndexedSeqBasics
import net.noresttherein.sugar.collections.LikeIterable.LikeIterableBasics
import net.noresttherein.sugar.collections.LikeIterableOnce.LikeIterableOnceBuilder
import net.noresttherein.sugar.collections.LikeIterableOnce.Generic.Template
import net.noresttherein.sugar.collections.extensions.{IterableOnceExtension, SeqExtension, StringExtension, mutableIndexedSeqExtension}
import net.noresttherein.sugar.collections.util.{elementsToCopy, errorString}
import net.noresttherein.sugar.exceptions.{??!, noSuch_!, outOfBounds_!, unsupported_!}
import net.noresttherein.sugar.extensions.{IteratorCompanionExtension, IteratorExtension, PartialFunctionExtension, boxeqMethod}
import net.noresttherein.sugar.typist.{<::<, Unknown, kinds}
import net.noresttherein.sugar.reflect.extensions.ClassExtension
import net.noresttherein.sugar.typist.kinds.Any1
import net.noresttherein.sugar.util.SerializableSingleton
import net.noresttherein.sugar.vars.{Maybe, Opt}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One




/**
  * @define coll indexed sequence
  */
trait LikeIndexedSeq[+X, -Xs, +CC[_], +C] extends LikeSeq[X, Xs, CC, C] {
	override def isApplyFast(elems :Xs) :Boolean = true
//	override def view(elems :Xs) :SeqView[X]
	override def toSeq(elems :Xs) :Seq[X] = toIndexedSeq(elems)
//	override def toOps(elems :Xs) :collection.IndexedSeqOps[X, CC, C]
}




/** @define TypeClass `LikeIndexedSeq` */
private[collections] sealed abstract class Rank2LikeIndexedSeqs extends LikeIterableOnceSummons[LikeIndexedSeq] {
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeIndexedSeq[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank1LikeIndexedSeqs extends Rank2LikeIndexedSeqs {
	implicit final def forOps[X, Xs, CC[A], C]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[X] with collection.IndexedSeqOps[X, CC, C],
	                                   generic :CC <::< Iterable) :LikeIndexedSeq[X, Xs, CC, C] =
		prototype.asInstanceOf[LikeIndexedSeq[X, Xs, CC, C]]

	private[this] val prototype = new LikeIndexedSeq.ForOps[Any, Iterable, collection.IndexedSeq[Any]] {
		override def toString = "LikeIndexedSeq.forOps"
		private def readResolve :AnyRef = LikeIndexedSeq.forOps[Any, collection.IndexedSeq[Any], Iterable, collection.IndexedSeq[Any]]
	}
}


@SerialVersionUID(Ver)
object LikeIndexedSeq extends Rank1LikeIndexedSeqs {

	@inline implicit def likeMutableIndexedSeq[X, Xs, CC[_], C](implicit like :LikeMutableIndexedSeq[X, Xs, CC, C])
			:LikeIndexedSeq[X, Xs, CC, C] =
		like

	@inline implicit def likeRanking[X, Xs, CC[_], C]
	                                (implicit like :LikeRanking[X, Xs, CC, C]) :LikeIndexedSeq[X, Xs, CC, C] =
		like

	implicit val forString :LikeIndexedSeq[Char, String, IndexedSeq, String] = new ForString

	implicit def forArrayLike[E] :LikeIndexedSeq[E, ArrayLike[E], ArrayLike, ArrayLike[E]] =
		arrayLikePrototype.asInstanceOf[LikeIndexedSeq[E, ArrayLike[E], ArrayLike, ArrayLike[E]]]

	implicit def forRefArrayLike[E] :LikeIndexedSeq[E, RefArrayLike[E], RefArrayLike, RefArrayLike[E]] =
		refArrayLikePrototype.asInstanceOf[LikeIndexedSeq[E, RefArrayLike[E], RefArrayLike, RefArrayLike[E]]]

	implicit def forIArrayLike[E] :LikeIndexedSeq[E, IArrayLike[E], IArrayLike, IArrayLike[E]] =
		iArrayLikePrototype.asInstanceOf[LikeIndexedSeq[E, IArrayLike[E], IArrayLike, IArrayLike[E]]]

//	implicit def forArray[E] :LikeIndexedSeq[E, Array[E], RefArray, Array[E]] =
//		arrayPrototype.asInstanceOf[LikeIndexedSeq[E, Array[E], RefArray, Array[E]]]

	implicit def forIArray[E] :LikeIndexedSeq[E, IRefArray[E], IRefArray, IArray[E]] =
		iArrayPrototype.asInstanceOf[LikeIndexedSeq[E, IRefArray[E], IRefArray, IArray[E]]]

	implicit def forIRefArray[E] :LikeIndexedSeq[E, IRefArray[E], IRefArray, IRefArray[E]] =
		iRefArrayPrototype.asInstanceOf[LikeIndexedSeq[E, IRefArray[E], IRefArray, IRefArray[E]]]


	@inline def forArray[E] :LikeIndexedSeq[E, Array[E], RefArray, Array[E]] = LikeMutableIndexedSeq.forArray
	@inline def forRefArray[E] :LikeIndexedSeq[E, RefArray[E], RefArray, RefArray[E]] = LikeMutableIndexedSeq.forRefArray

	private[this] val arrayLikePrototype = new LikeSeqForArrayLike[Unknown, ArrayLike] {
		override def toIterable(elems :ArrayLike[Unknown]) = ArrayLike.Wrapped(elems)
		override def toIndexedSeq(elems :ArrayLike[Unknown]) = IRefArray.Wrapped(IRefArray.copyOf(elems))
		override def infoString(elems :ArrayLike[Unknown]) :String =
			if (elems.getClass.getComponentType == classOf[Any]) "ArrayLike|" + elems.length + "|"
			else "ArrayLike[" + elems.getClass.getComponentType.name + "]|" + elems.length + "|"

		override def toString = "LikeIndexedSeq.forArrayLike"
		private def readResolve :AnyRef = LikeIndexedSeq.forArrayLike
	}
	private[this] val refArrayLikePrototype = new LikeSeqForRefArrayLike[Unknown, RefArrayLike] {
		override def toIterable(elems :RefArrayLike[Unknown]) :Iterable[Unknown] = RefArrayLike.Wrapped(elems)
		override def toIndexedSeq(elems :RefArrayLike[Unknown]) :IndexedSeq[Unknown] =
			IRefArray.Wrapped(IRefArray.copyOf(elems))
		override def infoString(elems :RefArrayLike[Unknown]) :String = "RefArrayLike|" + elems.length + '|'
		override def toString = "LikeIndexedSeq.forRefArrayLike"
		private def readResolve :AnyRef = LikeIndexedSeq.forRefArrayLike
	}
	private[this] val iArrayLikePrototype = new LikeSeqForArrayLike[Unknown, IArrayLike] {
		override def toIndexedSeq(elems :IArrayLike[Unknown]) = IArrayLike.Wrapped(elems)
		override def infoString(elems :IArrayLike[Unknown]) :String =
			if (elems.getClass.getComponentType == classOf[Any]) "IArrayLike|" + elems.length + "|"
			else "IArrayLike[" + elems.getClass.getComponentType.name +  "]|" + elems.length + "|"

		override def toString = "LikeIndexedSeq.forIArrayLike"
		private def readResolve :AnyRef = LikeIndexedSeq.forIArrayLike
	}
//	private[this] val arrayPrototype = new LikeSeqForArrayLike[Unknown, Array] {
//		override def toIterable(elems :Array[Unknown]) = ArrayFactory.Wrapped(elems)
//		override def toIndexedSeq(elems :Array[Unknown]) = IArray.Wrapped(elems.toIArray)
//		override def toString = "LikeIndexedSeq.forArray"
//		private def readResolve :AnyRef = LikeMutableIndexedSeq.forArray
//	}
	private[this] val iArrayPrototype = new LikeSeqForArrayLike[Unknown, IArray] {
		override def toIndexedSeq(elems :IArray[Unknown]) = IArray.Wrapped(elems)
		override def infoString(elems :IArray[Unknown]) :String =
			"IArray[" + elems.getClass.name + "]|" + elems.length + "|"

		override def toString = "LikeIndexedSeq.forIArray"
		private def readResolve :AnyRef = LikeIndexedSeq.forIArray
	}
	private[this] val iRefArrayPrototype = new LikeSeqForArrayLike[Unknown, IRefArray] {
		override def toIndexedSeq(elems :IRefArray[Unknown]) = IRefArray.Wrapped(elems)
		override def infoString(elems :IRefArray[Unknown]) :String = "IRefArray|" + elems.length + "|"
		override def toString = "LikeIndexedSeq.forIRefArray"
		private def readResolve :AnyRef = LikeIndexedSeq.forIRefArray
	}


	def adapt[X, Xs](elems :Xs)(implicit likeSeq :LikeIndexedSeq[X, Xs, Any1, Any]) :collection.IndexedSeq[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with LikeIterableAdapter[X, elems.type, collection.IndexedSeq, collection.IndexedSeq[X]]
			with collection.IndexedSeq[X]
			with LikeSeqAdapter[X, elems.type, collection.IndexedSeq, collection.IndexedSeq[X]]
		{
			override val ops = likeSeq.specific(elems)
			override def iterator :Iterator[X] = ops.iterator(elems)
		}

	trait Generic[CC[_]] extends LikeSeq.Generic[CC] with Template[CC, LikeIndexedSeq]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		@inline implicit def likeRanking[CC[_]](implicit generic :LikeRanking.Generic[CC]) :Generic[CC] = generic
		@inline implicit def likeMutableIndexedSeq[CC[_]](implicit generic :LikeMutableIndexedSeq.Generic[CC])
				:Generic[CC] =
			generic

		implicit def forArrayLike  :Generic[ArrayLike] = arrayLike
		implicit def forIArrayLike :Generic[IArrayLike] = iArrayLike
		implicit def forRefArrayLike :Generic[RefArrayLike] = refArrayLike
		//Can't have Generic[IArray] because mapping requires a ClassTag to produce another IArray
//		implicit def forIArray     :Generic[IArray] = iArray

		@SerialVersionUID(Ver)
		private[this] object arrayLike extends Generic[ArrayLike] {
			implicit override def of[X] :LikeIndexedSeq[X, ArrayLike[X], ArrayLike, ArrayLike[X]] =
				LikeIndexedSeq.forArrayLike

			override def toString = "LikeIndexedSeq.Generic.forArrayLike"
		}
		@SerialVersionUID(Ver)
		private[this] object iArrayLike extends Generic[IArrayLike] {
			implicit override def of[X] :LikeIndexedSeq[X, IArrayLike[X], IArrayLike, IArrayLike[X]] =
				LikeIndexedSeq.forIArrayLike

			override def toString = "LikeIndexedSeq.Generic.forIArrayLike"
		}
//		@SerialVersionUID(Ver)
//		private[this] object iArray extends Generic[IArray] {
//			implicit override def of[X] :LikeIndexedSeq[X, IArray[X], IArray, IArray[X]] =
//				LikeIndexedSeq.forIArray
//
//			override def toString = "LikeIndexedSeq.Generic.forIArray"
//		}
		@SerialVersionUID(Ver)
		private[this] object refArrayLike extends Generic[RefArrayLike] {
			implicit override def of[X] :LikeIndexedSeq[X, RefArrayLike[X], RefArrayLike, RefArrayLike[X]] =
				LikeIndexedSeq.forRefArrayLike

			override def toString = "LikeIndexedSeq.Generic.forRefArrayLike"
		}
	}

	private[LikeIndexedSeq] sealed abstract class Rank1Generics {
		implicit final def forOps[CC[X] <: Iterable[X] with collection.IndexedSeqOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeIndexedSeq.Generic.forOps", Generic.forOps[IndexedSeq])
				with Generic[IndexedSeq]
			{
				implicit override def of[X] :LikeIndexedSeq[X, IndexedSeq[X], IndexedSeq, IndexedSeq[X]] =
					LikeIndexedSeq.forOps
			}
	}


	//todo:
/*
	def unapply[A](items :IterableOnce[A]) :Maybe[LikeIndexedSeq[A, items.type, Any1, _]] = items match {
		case seq     :collection.IndexedSeqOps[A, kinds.Any1, _] =>
			Yes(forOps[A, IndexedSeq, IndexedSeq, IndexedSeq[A]].asInstanceOf[LikeIndexedSeq[A, items.type, Any1, _]])
		case ranking :Ranking[A]                                 =>
			Yes(LikeRanking.forOps[A, Ranking[A], Ranking, Ranking[A]].asInstanceOf[LikeRanking[A, items.type, Any1, _]])
		case set     :IndexedSet[A]                              =>
			Yes(???)
		case slice   :ArrayIterableOnce[A] =>
			val from  = slice.startIndex
			val until = from + slice.knownSize
			Yes(???)
		case _ => No
	}
*/

	trait LikeIndexedSeqBasics[+X, -Xs, +CC[_], +C]
		extends LikeSeq.OfKnownSize[X, Xs, CC, C] with LikeIndexedSeq[X, Xs, CC, C]
	{ ops =>
		override def isApplyFast(elems :Xs) :Boolean = true

//		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = {
//			var i = from; val len = size(elems)
//			while (i < len && p(apply(elems, i)))
//				i += 1
//			i - from
//		}

		override def last(elems :Xs) :X = {
			val length = size(elems)
			if (length == 0) noSuch_!(infoString(elems) + ".head")
			else apply(elems, length - 1)
		}

//		override def tail(elems :Xs) :C = {
//			val length = size(elems)
//			if (length == 0) unsupported_!(infoString(elems) + "().tail")
//			else slice(elems, 1, length)
//		}
//		override def init(elems :Xs) :C = {
//			val length = size(elems)
//			if (length == 0) unsupported_!(infoString(elems) + "().init")
//			else slice(elems, 0, length - 1)
//		}
//		override def drop(elems :Xs, n :Int) :C = slice(elems, n, size(elems))
//		override def take(elems :Xs, n :Int) :C = slice(elems, 0, n)
//		override def dropRight(elems :Xs, n :Int) :C = slice(elems, 0, size(elems) - n)
//		override def takeRight(elems :Xs, n :Int) :C = { val end = size(elems); slice(elems, end - n, end) }
//		override def takeWhile(elems :Xs)(p :X => Boolean) :C = take(elems, segmentLength(elems)(p))
//		override def dropWhile(elems :Xs)(p :X => Boolean) :C = drop(elems, segmentLength(elems)(p))
////		override def splitAt(elems :Xs, n :Int) :(C, C) = (take(elems, n), drop(elems, n))
//		override def span(elems :Xs)(p :X => Boolean) :(C, C) = {
//			val i = segmentLength(elems)(p)
//			(take(elems, i), drop(elems,i))
//		}


//	protected override def reversed(elems :Xs) :Iterable[X] = new IndexedSeqView.Reverse(toOps(elems))

//	override def reverse(elems :Xs) :C = fromSpecific(elems)(reversed(elems))

//		override def iterator(elems :Xs) :Iterator[X] =
//			new AbstractIndexedIterator[X](0, size(elems)) {
//				override def head :X = apply(elems, index)
//			}
//		override def reverseIterator(elems :Xs) :Iterator[X] = //toOps(elems).reverseIterator //view(elems).reverseIterator
//			new AbstractReverseIndexedIterator[X](0, size(elems)) {
//				override def head :X = apply(elems, index)
//			}

//		override def view(elems :Xs) :IndexedSeqView[X] =
//			new LikeSeqAdapterView[X, Xs](elems, this) with IndexedSeqView[X]
	}


	//todo: switch between iterator and apply depending on isApplyPreferred
/*
	trait LikeIndexedSeqBuilder[X, -Xs, +CC[_], +C]
		extends LikeIndexedSeqBasics[X, Xs, CC, C] with LikeSeq.FromApply[X, Xs, CC, C]
		   with LikeIterableOnceBuilder[X, Xs, CC, C]
	{
		override def reverse(elems :Xs) :C =
			if (isEmpty(elems))
				empty(elems)
			else {
				val res = specificBuilder(elems)
				var i   = size(elems)
				res sizeHint i
				while (i > 0) {
					i -= 1
					res addOne apply(elems, i)
				}
				res.result()
			}

		override def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C =
			if (isEmpty(elems))
				empty(elems)
			else {
				val array = toArray(elems)
				array.sortInPlace[U]()
				(specificBuilder(elems) ++= ArrayIterator(array)).result()
			}

		/*{
			val length = size(elems)
			if (index < 0 | index >= length)
				outOfBounds_!(toString + ".updated(" + infoString(elems) + ", " + index + ", _)")
			val res = genericBuilder[U](elems)
			res sizeHint length
			var i = 0
			var until = index
			while (i < until) {
				while (i < index) {
					res addOne apply(elems, i)
					i += 1
				}
				if (i < length) {
					res addOne elem
					i = index + 1
				}
				until = length
			}
			res.result()
		}
*/
		override def updated[U >: X](elems :Xs, index :Int, elem :U) :CC[U] = {
			val length = size(elems)
			if (index < 0 | index >= size)
				outOfBounds_!(toString + ".updated(" + infoString(elems) + ", " + index + ", _)")
			patch(elems, index, elem, 1, true)
		}
		override def prepended[U >: X](elems :Xs, elem :U) :CC[U] = patch(elems, 0, elem, 0, true)
		override def appended[U >: X](elems :Xs, elem :U) :CC[U] = patch(elems, size(elems), elem, 0, true)
//		override def inserted[U >: X](elems :Xs, index :Int, elem :U) :CC[U] = {
//			val length = size(elems)
//			if (index < 0 | index > size)
//				outOfBounds_!(toString + ".updated(" + infoString(elems) + ", " + index + ", _)")
//			patch(elems, index, elem, 0)
//		}

		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			this.patch(elems, index, patch, -1, true)

		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			this.patch(elems, index, patch, -1, false)

		override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			patch(elems, 0, prefix, 0, true)

		override def concat[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			patch(elems, size(elems), suffix, 0, true)

//		override def insertedAll[U >: X, O](elems :Xs, index :Int, that :O)
//		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
//		{
//			val length = size(elems)
//			if (index < 0 | index > size)
//				outOfBounds_!(toString + ".updated(" + infoString(elems) + ", " + index + ", _)")
//			patch(elems, index, that, 0)
//		}

		override def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                             (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			patch[U, O](elems, from, other, math.max(0, replaced), false)

		private def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int, validateIndices :Boolean)
		                            (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thisSize = size(elems)
			val thatSize = likeCollection.knownSize(other)
			if (index <)
		}


		override def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U] = {
			val length = size(elems)
			if (length >= len)
				toGeneric(elems)
			else {
				val res = genericBuilder[U](elems)
				res sizeHint len
				var i = 0
				while (i < length) {
					res addOne apply(elems, i)
					i += 1
				}
				while (i < len) {
					res addOne elem
					i += 1
				}
				res.result()
			}
		}
	}
*/



	trait FromOps[+X, -Xs, +CC[_], +C] extends LikeIndexedSeq[X, Xs, CC, C] with LikeSeq.FromOps[X, Xs, CC, C] {
		override def view(elems :Xs) :IndexedSeqView[X] = toOps(elems).view
		override def toOps(elems :Xs) :collection.IndexedSeqOps[X, CC, C]
	}

	trait ForOps[X, CC[A] <: Iterable[A], C <: CC[X] with collection.IndexedSeqOps[X, CC, C]]
		extends FromOps[X, C, CC, C] with LikeSeq.ForOps[X, CC, C]
	{
		override def toOps(elems :C) :collection.IndexedSeqOps[X, CC, C] = elems
	}

	trait FromIterator[X, -Xs, +CC[_], +C] extends LikeSeq.FromIterator[X, Xs, CC, C] with LikeIndexedSeq[X, Xs, CC, C]


/*  //todo: FromBuilder
	trait FromBuilder[X, -Xs, +CC[_], +C]
		extends LikeIndexedSeqBasics[X, Xs, CC, C] with LikeSeq.FromApply[X, Xs, CC, C]
		   with LikeIterableOnce.LikeIterableOnceFactory[X, Xs, CC, C]
	{
		override def copy(elems :Xs) :C = {
			val length = size(elems)
			val res = specificBuilder(elems)
			res sizeHint length
			var i = 0
			while (i < length) {
				res += apply(elems, i)
				i   += 1
			}
			res.result()
		}

		override def reverse(elems :Xs) :C = makeSpecific(elems)(reverseIterator(elems))
		override def sorted[U >: X](elems :Xs)(implicit ord :Ordering[U]) :C =
			makeSpecific(elems)(TemporaryBuffer.from(elems).sortInPlace[U])

		override def partition(elems :Xs)(p :X => Boolean) :(C, C) = {
			val length = size(elems)
			if (length == 0)
				return (empty(elems), empty(elems))
			val l = specificBuilder(elems)
			val r = specificBuilder(elems)
			var i = 0
			while (i < length) {
				val next = apply(elems, i)
				if (p(next)) l += next
				else r += next
				i += 1
			}
			(l.result(), r.result())
		}

		override def zipWithIndex[U >: X](elems :Xs) :CC[(U, Int)] = {
			val length = size(elems)
			val res = genericBuilder[(U, Int)](elems)
			res sizeHint length
			var i = 0
			while (i < length) {
				res += (apply(elems, i), i)
				i  += 1
			}
			res.result()
		}
		override def zip[U >: X, A, O](elems :Xs, that :O)(implicit likeCollection :LikeCollection[A, O]) :CC[(U, A)] = {
			val res = genericBuilder[(U, A)](elems)
			val thisSize = size(this)
			val thatSize = likeCollection.knownSize(that)
			var i = 0
			if (thatSize >= 0) {
				val max = math.min(thisSize, thatSize)
				likeCollection match {
					case seq :LikeSeq[A, O, Any1, Any] if seq.isApplyPreferred(that) =>
						while (i < max) {
							res += (apply(elems, i), seq(that, i))
							i   += 1
						}
					case _ =>
						val those = likeCollection.iterator(that)
						while (i < max) {
							res += (apply(elems, i), those.next())
							i   += 1
						}
				}
			} else {
				val those = likeCollection.iterator(that)
				while (i < thisSize && those.hasNext) {
					res += (apply(elems, i), those.next())
					i   += 1
				}
			}
			res.result()
		}
		override def zipAll[U >: X, A, O](elems :Xs, that :O, thisElem :U, thatElem :A)
		                                 (implicit likeIterable :LikeIterable[A, O, Any1, _]) :CC[(U, A)] =
		{
			val thisSize = size(elems)
			val thatSize = likeIterable.knownSize(that)
			val res = genericBuilder[(U, A)](elems)
			var i = 0
			likeIterable match {
				case seq :LikeSeq[A, O, Any1, _] if seq.isApplyPreferred(that) =>
					val min = math.min(thisSize, thatSize)
					while (i < min) {
						res += (apply(elems, i), seq(that, i))
						i   += 1
					}
					while (i < thisSize) {
						res += (apply(elems, i), thatElem)
						i   += 1
					}
					while (i < thatSize) {
						res += (thisElem, seq(that, i))
						i   += 1
					}
				case _ =>
					var those = likeIterable.iterator(that)
					while (i < thisSize) {
						while (i < thisSize && those.hasNext) {
							res += (apply(elems, i), those.next())
							i   += 1
						}
						if (i < thisSize)
							those = Iterator.const(thisSize - i)(thatElem)
					}
					while (those.hasNext)
						res += (thisElem, those.next())
			}
			res.result()
		}

		override def updated[U >: X](elems :Xs, index :Int, elem :U) :CC[U] =
			insertOrUpdate(elems, index, elem, 1, "updated")

		override def prepended[U >: X](elems :Xs, elem :U) :CC[U] = inserted(elems, 0, elem)
		override def appended[U >: X](elems :Xs, elem :U) :CC[U] = inserted(elems, size(elems), elem)
		def inserted[U >: X](elems :Xs, index :Int, elem :U) :CC[U] = insertOrUpdate(elems, index, elem, 0, "inserted")

		private def insertOrUpdate[U >: X](elems :Xs, index :Int, elem :U, skip :Int, method :String) :CC[U] = {
			val length = size(elems)
			if (index < 0 | index > length - skip)
				outOfBounds_!(toString + "." + method + "(" + infoString(elems) + ", " + index + ", _)")
			val res = genericBuilder[U](elems)
			res sizeHint length + 1
			var i = 0
			while (i < index) {
				res += apply(elems, i)
				i += 1
			}
			res += elem
			i += skip
			while (i < length) {
				res += apply(elems, i)
				i += 1
			}
			res.result()
		}

		override def updatedAll[U >: X, O](elems :Xs, index :Int, patch :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thisSize = size(elems)
			val thatSize = likeCollection.knownSize(patch)
			if (index < 0 | index > thisSize | thatSize >= 0 & index > thisSize - thatSize)
				outOfBounds_!(
					toString + ".updatedAll(" + infoString(elems) + ", " + index +
						", " + likeCollection.infoString(patch) + ")"
				)
			if (thatSize == 0)
				makeGeneric(elems)(toGeneric(elems)) //Because we might be mutable.
			val res = genericBuilder[U](elems)
			var i = 0
			while (i < index) {
				res += apply(elems, i)
				i   += 1
			}
			if (thatSize >= 0)
				likeCollection.addTo(patch, res) //We've already validated the range
			else {
				val rem =
					if (index > 0 && res.knownSize >= 0) {
						val (take, rem) = likeCollection.iterator(patch).splitAt(thisSize - index)
						res ++= take
						i = res.knownSize
						rem
					} else {
						val it = likeCollection.iterator(patch)
						while (it.hasNext && i < thisSize) {
							res += it.next()
							i   += 1
						}
						it
					}
					if (rem.hasNext)
						outOfBounds_!(
							"Attempted to update more than " + (thisSize - index) + " elements at " + index +
							"in " + infoString(elems) + "."
						)
					while (i < thisSize) {
						res += apply(elems, i)
						i   += 1
					}
				res.result()
			}
		}

		override def overwritten[U >: X, O](elems :Xs, index :Int, patch :O)
		                                   (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val length = size(elems)
			val thatSize = likeCollection.knownSize(patch)
			if (index == Int.MinValue | length == 0 | index >= length
				| thatSize == 0 | thatSize >= 0 & thatSize + index <= 0
			)
				makeGeneric(elems)(toGeneric(elems)) //Because we might be mutable
			else {
				val res = genericBuilder[U](elems)
				res sizeHint length
				var i = 0
				var until = math.max(0, math.min(length, index))
				while (i < length) { //runs at most twice
					while (i < until) {
						res += apply(elems, i)
						i   += 1
					}
					i += likeCollection.addTo(patch, res, length - until)
					until = length
				}
				res.result()
			}
		}

		override def prependedAll[U >: X, O](elems :Xs, prefix :O)
		                                    (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			prependOrAppend[U, O](elems, prefix, true)

		override def concat[U >: X, O](elems :Xs, suffix :O)(implicit likeCollection :LikeCollection[U, O]) :CC[U] =
			prependOrAppend[U, O](elems, suffix, false)

		private def prependOrAppend[U >: X, O](elems :Xs, that :O, prepend :Boolean)
		                                      (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thisSize = size(elems)
			val thatSize = likeCollection.knownSize(that)
			if (thatSize > Int.MaxValue - thisSize)
				outOfBounds_!("Cannot add " + thatSize + " elements to " + infoString(elems) + ": Int.MaxValue size exceeded.")
			if (thatSize == 0)
				toGeneric(elems)
			else if (thisSize == 0)
				makeGeneric(elems)(likeCollection.toIterableOnce(that))
			else {
				val res = genericBuilder[U](elems)
				if (thatSize >= 0)
					res sizeHint thatSize + thisSize
				if (prepend)
					likeCollection.addTo(that, res)
				addTo(elems, res)
				if (!prepend)
					likeCollection.addTo(that, res)
				res.result()
			}
		}

		override def patch[U >: X, O](elems :Xs, from :Int, other :O, replaced :Int)
		                             (implicit likeCollection :LikeCollection[U, O]) :CC[U] =
		{
			val thisSize = size(elems)
			val thatSize = likeCollection.knownSize(other)
			if (thisSize == 0 | thatSize == 0)
				makeGeneric(elems)(toGeneric(elems))
			else {
				val res = genericBuilder[U](elems)
				if (thatSize >= 0)
					res sizeHint thisSize + thatSize - replaced
				var until = math.min(thisSize, math.max(0, from))
				var i = 0
				while (i < thisSize) {
					while (i < until) {
						res += apply(elems, i)
						i   += 1
					}
					if (i < thisSize) {
						likeCollection.addTo(other, res)
						i += replaced
						until = thisSize
					}
				}
				res.result()
			}
		}

		override def padTo[U >: X](elems :Xs, len :Int, elem :U) :CC[U] = {
			val thisSize = size(elems)
			if (thisSize >= len)
				toGeneric(elems)
			else {
				val res = genericBuilder[U](elems)
				res sizeHint len
				addTo(elems, res)
				var i = thisSize
				while (i < len) {
					res += elem
					i   += 1
				}
				res.result()
			}
		}

		override def partitionMap[A1, A2](elems :Xs)(f :X => Either[A1, A2]) :(CC[A1], CC[A2]) = {
			val l = genericBuilder[A1]
		}

		override def groupBy[K](elems :Xs)(f :X => K) :Map[K, C] = ???

		override def groupMap[K, A](elems :Xs)(key :X => K)
		                           (f :X => A) :Map[K, CC[A]] = ???

		protected def specificBuilder(elems :Xs) :Builder[X, C]
		protected def genericBuilder[A](elems :Xs) :Builder[A, CC[A]]
	}
*/



	@nowarn("cat=unused")
	private trait ForArrayIterableOnce[+X, -Xs <: ArrayIterableOnce[X], +CC[_], C <: ArrayIterableOnce[X]]
		extends LikeIndexedSeqBasics[X, Xs, CC, C] with LikeSeq.FromApply[X, Xs, CC, C]
		   with LikeCollection.ForIterableOnce[X, Xs]
	{ this :LikeCollection.FromIterableOnceSeal =>
		override def isApplyPreferred(elems :Xs) :Boolean = true

//		override def knownSize(elems :Xs) :Int = elems.knownSize

		override def apply(elems :Xs, i :Int) :X = {
			if (i < 0 | i > elems.knownSize)
				outOfBounds_!(i, elems)
			elems.unsafeArray(elems.startIndex + i).asInstanceOf[X]
		}
		override def forall(elems :Xs)(p :X => Boolean) :Boolean = {
			val len = elems.knownSize
			ArrayLikeSpecOps.segmentLength(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, len)(p, 0) == len
		}
		override def exists(elems :Xs)(p :X => Boolean) :Boolean = {
			val array = elems.unsafeArray.asInstanceOf[Array[X]]
			ArrayLikeSpecOps.indexWhere(array, elems.startIndex, elems.knownSize)(p, 0) >= 0
		}
		override def count(elems :Xs)(p :X => Boolean) :Int =
			ArrayLikeSpecOps.count(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)(p)

		override def find(elems :Xs)(p :X => Boolean) :Option[X] =
			ArrayLikeSpecOps.find(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)(p)

		override def findLast(elems :Xs)(p :X => Boolean) :Option[X] =
			ArrayLikeSpecOps.findLast(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)(p)

		override def indexOf[U >: X](elems :Xs, from :Int, elem :U) :Int = {
			val array = elems.unsafeArray.asInstanceOf[Array[U]]
			ArrayLikeSpecOps.indexOf(array, elems.startIndex, elems.knownSize)(elem, from)
		}
		override def lastIndexOf[U >: X](elems :Xs, end :Int, elem :U) :Int = {
			val array = elems.unsafeArray.asInstanceOf[Array[U]]
			ArrayLikeSpecOps.lastIndexOf(array, elems.startIndex, elems.knownSize)(elem, end)
		}
		override def indexWhere(elems :Xs, from :Int)(p :X => Boolean) :Int = {
			val array = elems.unsafeArray.asInstanceOf[Array[X]]
			ArrayLikeSpecOps.indexWhere(array, elems.startIndex, elems.knownSize)(p, from)
		}
		override def lastIndexWhere(elems :Xs, end :Int)(p :X => Boolean) :Int = {
			val array = elems.unsafeArray.asInstanceOf[Array[X]]
			ArrayLikeSpecOps.lastIndexWhere(array, elems.startIndex, elems.knownSize)(p, end)
		}
		override def segmentLength(elems :Xs, from :Int)(p :X => Boolean) :Int = {
			val array = elems.unsafeArray.asInstanceOf[Array[X]]
			ArrayLikeSpecOps.segmentLength(array, elems.startIndex + from, elems.knownSize)(p, 0)
		}

		override def indexOfSlice[U >: X, O](elems :Xs, from :Int, that :O)
		                                    (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			//todo: specialized KMP in ArrayLikeSpecOps, and working on a slice
			KMP.indexOfSlice(elems, that, from)(this, likeSeq)

		override def lastIndexOfSlice[U >: X, O](elems :Xs, end :Int, that :O)
		                                        (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
			KMP.lastIndexOfSlice(elems, that, end)(this, likeSeq)

		override def startsWith[U >: X, O](elems :Xs, offset :Int, that :O)
		                                  (implicit likeCollection :LikeCollection[U, O]) :Boolean =
			likeCollection.knownSize(that) match {
				case  _ if offset < 0 => false
				case -1 if offset >= elems.knownSize => offset == elems.knownSize && likeCollection.isEmpty(that)
				case -1 => super.startsWith[U, O](elems, offset, that)
				case  0 => offset <= elems.knownSize
				case  n if offset > elems.knownSize - n => false
				case  n => likeCollection match {
					case ForArrayLike(toArray) =>
						ArrayLikeSpecOps.startsWith(
							elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex + offset, elems.knownSize - offset,
							toArray(that).asInstanceOf[Array[X]]
						)
					case ForArrayIterableOnce(unapply) =>
						val (array, start, length) = unapply(that)
						ArrayLikeSpecOps.startsWith(
							elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex + offset, elems.knownSize - offset,
							array.asInstanceOf[Array[X]], start, length
						)
					case _ => super.startsWith[U, O](elems, offset, that)
				}
			}

		override def search[U >: X](elems :Xs, elem :U, from :Int, until :Int)(implicit ord :Ordering[U]) :SearchResult =
			elems.unsafeArray.asInstanceOf[ArrayLike[X]].search(elem, from, until)

		override def foldLeft[A](elems :Xs)(z :A)(op :(A, X) => A) :A =
			ArrayLikeSpecOps.foldLeft(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)(z)(op)

		override def foldRight[A](elems :Xs)(z :A)(op :(X, A) => A) :A =
			ArrayLikeSpecOps.foldRight(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)(z)(op)

		override def copyToArray[U >: X](elems :Xs, array :Array[U], start :Int, max :Int) :Int =
			ArrayLike.permissiveCopy(elems.unsafeArray.asInstanceOf[ArrayLike[X]], elems.startIndex, array, start, max)

//		override def iterator(elems :Xs) :Iterator[X] =
//			ArrayLikeIterator(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)

		override def reverseIterator(elems :Xs) :Iterator[X] = {
			val array = elems.unsafeArray.asInstanceOf[Array[X]]
			ReverseArrayLikeIterator.slice(array, elems.startIndex, elems.startIndex + elems.knownSize)
		}
//		override def stepper[S <: Stepper[_]](elems :Xs)(implicit shape :StepperShape[X, S]) :S with EfficientSplit =
//			if (elems.knownSize == 0) Stepper.empty
//			else ArrayStepper(elems.unsafeArray.asInstanceOf[Array[X]], elems.startIndex, elems.knownSize)

		override def view(elems :Xs) :IndexedSeqView[X] =
			elems.unsafeArray.asInstanceOf[Array[X]].view.slice(elems.startIndex, elems.startIndex + elems.knownSize)

		override def toImpureSeq(elems :Xs) :collection.IndexedSeq[X] = elems match {
			case seq :collection.IndexedSeq[X] => seq
			case _ =>
				val offset = elems.startIndex
				ArrayLike.Slice(elems.unsafeArray.asInstanceOf[Array[X]], offset, offset + elems.knownSize)
		}
		override def toIndexedSeq(elems :Xs) :IndexedSeq[X] = elems match {
			case seq :IndexedSeq[X] => seq
			case _ =>
				val offset = elems.startIndex
				val size   = elems.knownSize
				if (elems.isImmutable)
					DefaultArraySeq.slice(elems.unsafeArray.asInstanceOf[IArray[X]], offset, offset + size)
				else
					DefaultArraySeq.wrap(IArray.copyOfRange(elems.unsafeArray.asInstanceOf[IArray[X]], offset, offset + size))
		}
	}


	private object ForArrayIterableOnce {
		def unapply[X, Xs](ops :LikeCollection[X, Xs]) :Maybe[Xs => (ArrayLike[X], Int, Int)] = ops match {
			case _ :ForArrayIterableOnce[X, ArrayIterableOnce[X], Any1, ArrayIterableOnce[X]] @unchecked =>
				Yes((
						(a :ArrayIterableOnce[X]) => (a.unsafeArray, a.startIndex, a.knownSize)
					).asInstanceOf[Xs => (ArrayLike[X], Int, Int)]
				)
			case _ => No
		}
	}
	private object ForArrayLike {
		def unapply[X, Xs](ops :LikeCollection[X, Xs]) :Maybe[Xs => ArrayLike[X]] = ops match {
			case _ :LikeSeqForArrayLike[X, ArrayLike] @unchecked => Yes(
				(identity _).asInstanceOf[Xs => ArrayLike[X]]
			)
			case _ => No
		}
	}


	//todo: LikeIndexedSeq[E, ArrayIterableOnce]
//	@SerialVersionUID(Ver)
//	private final class ForArrayIterableOnceOps[E, CC[X] <: IterableOnce[X], C <: ArrayIterableOnceOps[E, CC, C]]
//		extends IterableOnceLike.ForOps[E, CC, C] with ForArrayIterableOnce[E, CC, C]
//	{
//		override def toOps(elems :C) :collection.IndexedSeqOps[E, CC, C] = new StringAsSeq(elems)
//
//		private def readResolve :AnyRef = LikeIndexedSeq.forArrayIterableOnce
//		override def toString = "LikeIndexedSeq.forArrayIterableOnce"
//	}



	@SerialVersionUID(Ver)
	private final class ForString
		extends LikeIndexedSeqBasics[Char, String, IndexedSeq, String]
			with LikeSeq.FromApply[Char, String, IndexedSeq, String]
	{
		override def isApplyPreferred(elems :String) :Boolean = true
		override def knownSize(elems :String) :Int = elems.length
		override def size(elems :String) :Int = elems.length
		override def apply(elems :String, i :Int) :Char = elems.charAt(i)

		override def forall(elems :String)(p :Char => Boolean) :Boolean = elems.forall(p)
		override def exists(elems :String)(p :Char => Boolean) :Boolean = elems.exists(p)
		override def count(elems :String)(p :Char => Boolean) :Int = elems.count(p)
		override def find(elems :String)(p :Char => Boolean) :Option[Char] = elems.find(p)
		override def findLast(elems :String)(p :Char => Boolean) :Option[Char] = elems.findLast(p)
		override def collectFirst[A](elems :String)(pf :PartialFunction[Char, A]) :Option[A] = elems.collectFirst(pf)
		override def indexOf[A >: Char](elems :String, from :Int, elem :A) :Int = elems.indexOf(elem, from)
		override def lastIndexOf[A >: Char](elems :String, end :Int, elem :A) :Int = elems.lastIndexOf(elem, end)

		override def indexWhere(elems :String, from :Int)(p :Char => Boolean) :Int = elems.indexWhere(p, from)
		override def lastIndexWhere(elems :String, end :Int)(p :Char => Boolean) :Int = elems.lastIndexWhere(p, end)

		override def startsWith[U >: Char, O](elems :String, offset :Int, that :O)
		                                     (implicit likeCollection :LikeCollection[U, O]) :Boolean=
			that match {
				case _ if offset < 0 | offset > elems.length => false
				//Check the type class, in case someone made a LikeStringSlice or smth.
				case str :String if likeCollection.isInstanceOf[ForString] =>
					val len = str.length
					offset <= elems.length - len && {
						var i = 0
						while (i < len && elems.charAt(offset + i) == str.charAt(i))
							i += 1
						i == len
					}
				case _ => super.startsWith[U, O](elems, offset, that)
			}

		override def foldLeft[A](elems :String)(z :A)(op :(A, Char) => A) :A = elems.foldLeft(z)(op)
		override def foldRight[A](elems :String)(z :A)(op :(Char, A) => A) :A = elems.foldRight(z)(op)
		override def foreach[U](elems :String)(f :Char => U) :Unit = elems.foreach(f)


		override def tapEach[U](elems :String)(f :Char => U) :String = { elems.foreach(f); elems }
		override def filter(elems :String)(p :Char => Boolean) :String = elems.filter(p)
		override def partition(elems :String)(p :Char => Boolean) :(String, String) = elems.partition(p)

		override def empty(elems :String) :String = ""
		override def copy(elems :String) :String = elems
		override def slice(elems :String, from :Int, until :Int) :String = {
			val length = elems.length
			val from0 = math.min(math.max(0, from), length)
			val until0 = math.max(from, math.min(until, length))
			elems.substring(from0, until0)
		}
		override def reverse(elems :String) :String = elems.reverse
		override def sorted[U >: Char](elems :String)(implicit ord :Ordering[U]) :String = elems.sorted


		override def scanLeft[A](elems :String)(z :A)(op :(A, Char) => A) :IndexedSeq[A] = elems.scanLeft(z)(op)
		override def scanRight[A](elems :String)(z :A)(op :(Char, A) => A) :IndexedSeq[A] = elems.scanRight(z)(op)

		override def map[A](elems :String)(f :Char => A) :IndexedSeq[A] = elems.map(f)
		override def collect[A](elems :String)(pf :PartialFunction[Char, A]) :IndexedSeq[A] = elems.collect(pf)
		override def flatMapIterableOnce[A](elems :String)(f :Char => IterableOnce[A]) :IndexedSeq[A] = elems.flatMap(f)

		override def updated[U >: Char](elems :String, index :Int, elem :U) :IndexedSeq[U] = elem match {
			case c :Char => elems.updated(index, c)
			case _       =>
				val res = DefaultIndexedSeq.newBuilder[U]
				res sizeHint elems.length
				if (index > 0)
					res ++= StringIterator(elems, 0, index)
				res += elem
				if (index < elems.length - 1)
					res ++= StringIterator(elems, index + 1, elems.length - index - 1)
				res.result()
		}
		override def prepended[U >: Char](elems :String, elem :U) :IndexedSeq[U] = elems.prepended(elem)
		override def appended[U >: Char](elems :String, elem :U) :IndexedSeq[U] = elems.appended(elem)

		override def updatedAll[U >: Char, O](elems :String, index :Int, patch :O)
		                                     (implicit likeCollection :LikeCollection[U, O]) :IndexedSeq[U] =
			updatedSlice[U, O](elems, index, patch, 0, true)

		override def overwritten[U >: Char, O](elems :String, index :Int, patch :O)
		                                      (implicit likeCollection :LikeCollection[U, O]) :IndexedSeq[U] =
			updatedSlice[U, O](elems, index, patch, 0, false)

		@tailrec
		private def updatedSlice[U >: Char, O](dst :String, dstIdx :Int, src :O, srcIdx :Int, validate :Boolean)
		                                      (implicit likeCollection :LikeCollection[U, O]) :IndexedSeq[U] =
		{
			def oob() = outOfBounds_!(
				toString + ".updatedAll(" +
					infoString(dst) + ", " + dstIdx + ", " + likeCollection.infoString(src) + ")"
			)
			likeCollection.knownSize(src) match {
				case  _ if dstIdx < 0 =>
					if (validate) oob()
					else if (dstIdx == Int.MinValue) dst
					else updatedSlice[U, O](dst, 0, src, srcIdx - dstIdx, false)
				case -1 if dstIdx > dst.length =>
					if (validate) oob() else dst
				case  n if validate & n >= 0 && dstIdx > dst.length - n => oob()
				case  0       => dst
				case  srcSize =>
					val len   = dst.length
					val res   = new JStringBuilder(len)
					var i     = 0
					var until = dstIdx
					while (i < len) { //The loop runs at most twice
						while (i < until) {
							res append dst.charAt(i)
							i += 1
						}
						likeCollection match {
							case _ :ForString => //likeCollection.knownSize >= 0 in the first two cases.
								val that  = src.asInstanceOf[String]
								until = math.min(len, i + that.length)
								val shift = srcIdx - dstIdx
								while (i < until) {
									res append that.charAt(i + shift)
									i += 1
								}
							case likeSeq :LikeSeq[U, O, Any1, _] if likeSeq.isApplyPreferred(src) =>
								until = math.min(len, i + likeSeq.size(src))
								val shift = srcIdx - dstIdx
								while (i < until) {
									res append likeSeq(src, i + shift)
									i += 1
								}
							case _ if srcSize >= 0 =>
								val it = likeCollection.iterator(src)
								until  = math.min(len, i + srcSize)
								while (i < until) {
									res append it.next()
									i += 1
								}
							case _ =>
								val it = likeCollection.iterator(src)
								while (i < len && it.hasNext) {
									res append it.next()
									i += 1
								}
								if (validate && it.hasNext)
									oob()
						}
						until = len
					}
					res.toString
			}
		}

		override def prependedAll[U >: Char, O](elems :String, prefix :O)
		                                       (implicit likeCollection :LikeCollection[U, O]) :IndexedSeq[U] =
			likeCollection.knownSize(prefix) match {
				case  0 => elems.toIndexedSeq
//				case  1 => elems.prepended(likeCollection.head(prefix))
				case  _ => prefix match {
					case str :String        => str ++ elems
					case str :WrappedString => str.unwrap ++ elems
					case _                  => elems.prependedAll(likeCollection.toIterableOnce(prefix))
				}
			}
		override def concat[U >: Char, O](elems :String, suffix :O)
		                                 (implicit likeCollection :LikeCollection[U, O]) :IndexedSeq[U] =
			likeCollection.knownSize(suffix) match {
				case 0 => elems.toIndexedSeq
				case _ => suffix match {
					case str :String        => elems concat str
					case str :WrappedString => elems concat str.unwrap
					case _                  => elems concat likeCollection.toIterableOnce(suffix)
				}
			}

		override def patch[A >: Char, O](elems :String, from :Int, other :O, replaced :Int)
		                                (implicit likeCollection :LikeCollection[A, O]) :IndexedSeq[A] =
			elems.patch(from, likeCollection.toIterableOnce(other), replaced)

		override def padTo[A >: Char](elems :String, len :Int, elem :A) :IndexedSeq[A] =
			if (elems.length <= len) elems else elems.padTo(len, elem)

		override def zipWithIndex[U >: Char](elems :String) :IndexedSeq[(U, Int)] = elems.zipWithIndex

		override def zip[U >: Char, A, O](elems :String, that :O)
		                                 (implicit likeCollection :LikeCollection[A, O]) :IndexedSeq[(U, A)] =
			likeCollection.knownSize(that) match {
				case 0 => IndexedSeq.empty
				case _ => elems.zip(likeCollection.toIterableOnce(that))
			}
		override def zipAll[U >: Char, A, O](elems :String, that :O, thisElem :U, thatElem :A)
		                                    (implicit likeIterable :LikeIterable[A, O, Any1, _]) :IndexedSeq[(U, A)] =
			likeIterable.knownSize(that) match {
				case  0 => elems.zipAll(Vector.empty, thisElem, thatElem)
				case  _ => elems.zipAll(likeIterable.toIterable(that), thisElem, thatElem)
			}
//		override def unzip[A1, A2](elems :String)(implicit asPair :Char => (A1, A2)) :Nothing = ??!
//		override def unzip3[A1, A2, A3](elems :String)(implicit asTriple :Char => (A1, A2, A3)) :Nothing = ??!

		override def partitionMap[A1, A2](elems :String)
		                                 (f :Char => Either[A1, A2]) :(IndexedSeq[A1], IndexedSeq[A2]) =
//			elems.partitionMap(f)
		{
			val l = IndexedSeq.newBuilder[A1]
			val r = IndexedSeq.newBuilder[A2]
			val len = elems.length
			var i = 0
			while (i < len) {
				f(elems.charAt(i)) match {
					case Left(a1)  => l += a1
					case Right(a2) => r += a2
				}
				i += 1
			}
			(l.result(), r.result())
		}

		override def groupBy[K](elems :String)(f :Char => K) :Map[K, String] = elems.groupBy(f)

		override def groupMap[K, A](elems :String)(key :Char => K)(f :Char => A) :Map[K, IndexedSeq[A]] =
			elems.groupMap(key)(f)

		override def groupMapReduce[K, A](elems :String)(key :Char => K)(f :Char => A)(reduce :(A, A) => A) :Map[K, A] =
			elems.groupMapReduce(key)(f)(reduce)

		override def withFilter(elems :String)(p :Char => Boolean) :WithFilter[Char, IndexedSeq] =
			new WrappedString(elems).withFilter(p)


		override def insertInto[U >: Char](elems :String, buffer :Buffer[U], index :Int) :Unit =
			buffer.insertAll(index, elems)

		override def patchOver[U >: Char](elems :String, buffer :Buffer[U], index :Int, replaced :Int) :Unit =
			buffer.patchInPlace(index, elems, replaced)


		override def copyToArray[U >: Char](elems :String, array :Array[U], start :Int, max :Int) :Int =
			copyRangeToArray[U](elems, array, start, 0, max)

		def copyRangeToArray[U >: Char](elems :String, xs :Array[U], start :Int, from :Int, len :Int) :Int = {
			val copied = util.elementsToCopy(elems.length, from, xs, start, len)
			xs match {
				case chars :Array[Char] =>
					elems.getChars(start, start + copied, chars, start)
				case _ =>
					val shift = start - from
					if (copied > 16)
						xs.updateAll(start, start + copied) { i :Int => elems.charAt(i - shift) }
					else {
						val end = from + copied
						var i = from
						while (i < end) {
							xs(i + shift) = elems.charAt(i)
							i += 1
						}
					}
			}
			copied
		}

		override def cyclicCopyToArray[U >: Char](elems :String, array :Array[U], index :Int, max :Int) :Int =
			if (max <= 0 || array.length == 0 || elems.length == 0)
				0
			else {
				val dstLen = array.length
				val start  = index % dstLen
				val copied = copyRangeToArray[U](elems, array, start, 0, max)
				if (copied == dstLen - start)
					copied + copyRangeToArray[U](elems, array, 0, copied, max - copied)
				else
					copied
			}

		override def iterator(elems :String) :Iterator[Char] = StringIterator(elems)
		override def reverseIterator(elems :String) :Iterator[Char] = ReverseStringIterator(elems)
		override def stepper[S <: Stepper[_]](elems :String)(implicit shape :StepperShape[Char, S]) :S =
			new StringStepper(elems).asInstanceOf[S]

		override def view(elems :String) :StringView = elems.view
		override def toGeneric[U >: Char](elems :String) :IndexedSeq[U] = elems
		override def toSpecific(elems :String) :String = elems
		override def toImpureSeq(elems :String) :collection.Seq[Char] = elems
		override def toIndexedSeq(elems :String) :IndexedSeq[Char] = elems
		override def toIterableOnce(elems :String) :IterableOnce[Char] = StringIterator(elems)
		override def toIterableOnceOps(elems :String) :IterableOnceOps[Char, Any, Any] = StringIterator(elems)
		override def to[C1](elems :String)(factory :Factory[Char, C1]) :C1 = factory.fromSpecific(elems)

		private def readResolve :AnyRef = LikeIndexedSeq.forString
		override def toString = "LikeIndexedSeq.forString"
	}

}






private abstract class LikeSeqForArrayLike[X, Arr[x] <: ArrayLike[x]]
	extends LikeIndexedSeqBasics[X, Arr[X], ArrayLike, Arr[X]] with LikeSeq.FromApply[X, Arr[X], ArrayLike, Arr[X]]
{
	//shadow the inherited Arr[E] => IterableOnce[E] conversion
	import net.noresttherein.sugar.arrays.{ArrayLikeExtension => conversion}

	override def knownSize(elems :Arr[X]) :Int = elems.length
	override def size(elems :Arr[X]) :Int = elems.length
	override def apply(elems :Arr[X], i :Int) :X = elems(i)

	final override def forall(elems :Arr[X])(p :X => Boolean) :Boolean = elems.forall(p)
	final override def exists(elems :Arr[X])(p :X => Boolean) :Boolean = elems.exists(p)
	final override def count(elems :Arr[X])(p :X => Boolean) :Int = elems.count(p)
	final override def find(elems :Arr[X])(p :X => Boolean) :Option[X] = elems.find(p)
	final override def findLast(elems :Arr[X])(p :X => Boolean) :Option[X] = elems.findLast(p)
	final override def indexOf[A >: X](elems :Arr[X], from :Int, elem :A) :Int = elems.indexOf(elem, from)
	final override def lastIndexOf[A >: X](elems :Arr[X], end :Int, elem :A) :Int = elems.lastIndexOf(elem, end)

	final override def indexWhere(elems :Arr[X], from :Int)(p :X => Boolean) :Int = elems.indexWhere(p)
	final override def lastIndexWhere(elems :Arr[X], end :Int)(p :X => Boolean) :Int =
		elems.lastIndexWhere(p, end)

	final override def segmentLength(elems :Arr[X], from :Int)(p :X => Boolean) :Int =
		elems.segmentLength(p)

	final override def startsWith[U >: X, O](elems :Arr[X], offset :Int, that :O)
	                                        (implicit likeCollection :LikeCollection[U, O]) :Boolean =
		likeCollection match {
			case _ :LikeSeqForArrayLike[X, ArrayLike] @unchecked =>
				elems.startsWith(that.asInstanceOf[ArrayLike[X]], offset)
			case _ =>
				elems.startsWith(likeCollection.toIterableOnce(that), offset)
		}
	final override def indexOfSlice[U >: X, O](elems :Arr[X], from :Int, that :O)
	                                          (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
		likeSeq match {
			case _ :LikeSeqForArrayLike[X, ArrayLike] @unchecked =>
				elems.indexOfSlice(that.asInstanceOf[ArrayLike[U]], from)
			case _ =>
				elems.indexOfSlice(likeSeq.toImpureSeq(that), from)
		}
	final override def lastIndexOfSlice[U >: X, O](elems :Arr[X], end :Int, that :O)
	                                              (implicit likeSeq :LikeSeq[U, O, Any1, _]) :Int =
		likeSeq match {
			case _ :LikeSeqForArrayLike[X, ArrayLike] @unchecked =>
				elems.lastIndexOfSlice(that.asInstanceOf[ArrayLike[U]], end)
			case _ =>
				elems.lastIndexOfSlice(likeSeq.toImpureSeq(that), end)
		}

	final override def sameElements[U >: X, O](elems :Arr[X], that :O)
	                                          (implicit likeCollection :LikeCollection[U, O]) :Boolean =
		likeCollection match {
			case _ :LikeSeqForArrayLike[X, ArrayLike] @unchecked =>
				elems.sameElements(that.asInstanceOf[ArrayLike[_]])
			case _ =>
				elems.sameElements(likeCollection.toIterableOnce(that))
		}

	final override def corresponds[A, O](elems :Arr[X], that :O)(p :(X, A) => Boolean)
	                                    (implicit likeCollection :LikeCollection[A, O]) :Boolean =
		likeCollection match {
			case _ :LikeSeqForArrayLike[A, ArrayLike] @unchecked =>
				elems.corresponds(that.asInstanceOf[ArrayLike[A]])(p)
			case _ =>
				elems.corresponds(likeCollection.toIterableOnce(that))(p)
		}

	final override def collectFirst[A](elems :Arr[X])(pf :PartialFunction[X, A]) :Option[A] = elems.collectFirst(pf)

	final override def search[A >: X :Ordering](elems :Arr[X], elem :A, from :Int, until :Int) :SearchResult =
		elems.search(elem, from, until)

	final override def foldLeft[A](elems :Arr[X])(z :A)(op :(A, X) => A) :A = elems.foldLeft(z)(op)
	final override def foldRight[A](elems :Arr[X])(z :A)(op :(X, A) => A) :A = elems.foldRight(z)(op)
	final override def reduceLeft[A >: X](elems :Arr[X])(op :(A, X) => A) :A = elems.reduceLeft(op)
	final override def reduceRight[A >: X](elems :Arr[X])(op :(X, A) => A) :A = elems.reduceRight(op)
	final override def reduceLeftOption[A >: X](elems :Arr[X])(op :(A, X) => A) :Option[A] =
		elems.reduceLeftOption(op)
	final override def reduceRightOption[A >: X](elems :Arr[X])(op :(X, A) => A) :Option[A] =
		elems.reduceRightOption(op)

	final override def foreach[U](elems :Arr[X])(f :X => U) :Unit = elems.foreach(f)

	final override def tapEach[U](elems :Arr[X])(f :X => U) :Arr[X] = elems.tapEach(f)


	final override def copy(elems :Arr[X]) :Arr[X] = ArrayLike.copyOf(elems)
	final override def slice(elems :Arr[X], from :Int, until :Int) :Arr[X] = elems.slice(from, until)
	final override def take(elems :Arr[X], n :Int) :Arr[X] = elems.take(n)
	final override def drop(elems :Arr[X], n :Int) :Arr[X] = elems.drop(n)
	final override def takeRight(elems :Arr[X], n :Int) :Arr[X] = elems.takeRight(n)
	final override def dropRight(elems :Arr[X], n :Int) :Arr[X] = elems.dropRight(n)
	final override def takeWhile(elems :Arr[X])(p :X => Boolean) :Arr[X] = elems.takeWhile(p)
	final override def dropWhile(elems :Arr[X])(p :X => Boolean) :Arr[X] = elems.dropWhile(p)
	final override def splitAt(elems :Arr[X], n :Int) :(Arr[X], Arr[X]) = elems.splitAt(n)
	final override def span(elems :Arr[X])(p :X => Boolean) :(Arr[X], Arr[X]) = elems.span(p)
	final override def empty(elems :Arr[X]) :Arr[X] =
		ArrayFactory.empty(elems.getClass.getComponentType).asInstanceOf[Arr[X]]

	final override def reverse(elems :Arr[X]) :Arr[X] = elems.reverse

	final override def sorted[A >: X :Ordering](elems :Arr[X]) :Arr[X] = elems.sorted[A]
	final override def sortWith(elems :Arr[X])(lt :(X, X) => Boolean) :Arr[X] = elems.sortWith(lt)
	final override def sortBy[A :Ordering](elems :Arr[X])(f :X => A) :Arr[X] = elems.sortBy(f)
	final override def distinct(elems :Arr[X]) :Arr[X] = elems.distinct
	final override def distinctBy[A](elems :Arr[X])(f :X => A) :Arr[X] = elems.distinctBy(f)

	final override def tail(elems :Arr[X]) :Arr[X] = elems.tail
	final override def init(elems :Arr[X]) :Arr[X] = elems.init

	final override def filter(elems :Arr[X])(p :X => Boolean) :Arr[X] = elems.filter(p)
	final override def filterNot(elems :Arr[X])(p :X => Boolean) :Arr[X] = elems.filterNot(p)
	final override def partition(elems :Arr[X])(p :X => Boolean) :(Arr[X], Arr[X]) = elems.partition(p)

	override def partitionMap[A1, A2](elems :Arr[X])(f :X => Either[A1, A2]) :(ArrayLike[A1], ArrayLike[A2]) =
		elems.length match {
			case 0 => (IRefArray.empty, IRefArray.empty)
			case 1 => f(elems(0)) match {
				case Left(a1)  => (IRefArray.one(a1), IRefArray.empty)
				case Right(a2) => (IRefArray.empty, IRefArray.one(a2))
			}
			case thisLength =>
				val left  = IRefArray.newBuilder[A1]
				val right = IRefArray.newBuilder[A2]
				var i = 0
				while (i < thisLength) {
					f(elems(i)) match {
						case Left(a1)  => left += a1
						case Right(a2) => right += a2
					}
					i += 1
				}
				(left.result(), right.result())
		}
	override def map[A](elems :Arr[X])(f :X => A) :ArrayLike[A] = {
		val len = elems.length
		val res = RefArray.ofDim[A](len)
		var i = 0
		while (i < len) {
			res(i) = f(elems(i))
			i += 1
		}
		res
	}
	override def flatMapIterableOnce[A](elems :Arr[X])(f :X => IterableOnce[A]) :ArrayLike[A] =
		if (elems.length == 0)
			IRefArray.empty
		else {
			val len = elems.length
			val res = IRefArray.newBuilder[A]
			var i = 0
			while (i < len) {
				res ++= f(elems(i))
				i += 1
			}
			res.result()
		}
	override def collect[A](elems :Arr[X])(pf :PartialFunction[X, A]) :ArrayLike[A] =
		if (elems.length == 0)
			IRefArray.empty
		else {
			val length = elems.length
			val res = IRefArray.newBuilder[A]
			var i = 0
			while (i < length) {
				val opt = pf.applyAndThenEither[Opt[A]](elems(i), One.apply _, _ => None)
				if (opt.isDefined)
					res += opt.get
				i += 1
			}
			res.result()
		}

	override def scanLeft[A](elems :Arr[X])(z :A)(op :(A, X) => A) :ArrayLike[A] = {
		val thisLength = elems.length
		val res = RefArray.ofDim[A](thisLength + 1)
		var last = z
		var i  = 0
		res(0) = z
		while (i < thisLength) {
			last = op(last, elems(i))
			i += 1
			res(i) = last
		}
		res
	}
	override def scanRight[A](elems :Arr[X])(z :A)(op :(X, A) => A) :ArrayLike[A] = {
		val thisLength = elems.length
		val res = RefArray.ofDim[A](thisLength + 1)
		var last = z
		res(thisLength) = z
		var i = thisLength - 1
		while (i < thisLength) {
			last = op(elems(i), last)
			res(i) = last
			i += 1
		}
		res
	}

	final override def zipWithIndex[U >: X](elems :Arr[X]) :Arr[(U, Int)] = elems.asInstanceOf[Arr[U]].zipWithIndex
	final override def zip[U >: X, A, O](elems :Arr[X], that :O)
	                                    (implicit likeCollection :LikeCollection[A, O]) :Arr[(U, A)] =
		elems.asInstanceOf[Arr[U]].zip(likeCollection.toIterableOnce(that))

	final override def zipAll[U >: X, A, O](elems :Arr[X], that :O, thisElem :U, thatElem :A)
	                                       (implicit likeIterable :LikeIterable[A, O, Any1, _]) :Arr[(U, A)] =
		elems.zipAll(likeIterable.toIterable(that), thisElem, thatElem)

	final override def unzip[A1, A2](elems :Arr[X])(implicit asPair :X => (A1, A2)) :(ArrayLike[A1], ArrayLike[A2]) =
		elems.length match {
			case      0 => (IRefArray.empty, IRefArray.empty)
			case length =>
				val array1 = RefArray.ofDim[A1](length)
				val array2 = RefArray.ofDim[A2](length)
				var i = 0
				while (i < length) {
					val (a1, a2) = asPair(elems(i))
					array1(i) = a1
					array2(i) = a2
					i += 1
				}
				(array1, array2)
		}
	final override def unzip3[A1, A2, A3](elems :Arr[X])(implicit asTriple :X => (A1, A2, A3))
			:(ArrayLike[A1], ArrayLike[A2], ArrayLike[A3]) =
		elems.length match {
			case      0 => (IRefArray.empty, IRefArray.empty, IRefArray.empty)
			case length =>
				val array1 = RefArray.ofDim[A1](length)
				val array2 = RefArray.ofDim[A2](length)
				val array3 = RefArray.ofDim[A3](length)
				var i = 0
				while (i < length) {
					val (a1, a2, a3) = asTriple(elems(i))
					array1(i) = a1
					array2(i) = a2
					array3(i) = a3
					i += 1
				}
				(array1, array2, array3)
		}

	final override def prepended[U >: X](elems :Arr[X], elem :U) :ArrayLike[U] = {
		val res =
			if (elem.getClass <%< elems.getClass.getComponentType)
				Array.like(elems.asInstanceOf[Array[U]], elems.length + 1)
			else
				new Array[Any](elems.length + 1).asInstanceOf[Array[U]]
		res(0) = elem
		ArrayLike.copy(elems, 0, res, 1, elems.length)
		res.asInstanceOf[ArrayLike[U]]
	}
	final override def appended[A >: X](elems :Arr[X], elem :A) :ArrayLike[A] = {
		val length = elems.length
		if (elem.getClass <%< elems.getClass.getComponentType) {
			val res = ArrayFactory.copyOf(elems.asInstanceOf[Array[A]], length + 1)
			res(length) = elem
			res
		} else {
			val res = RefArray.ofDim[A](length + 1)
			ArrayLike.copy(elems, 0, res, 0, length)
			res(length) = elem
			res
		}
	}
	final override def updated[U >: X](elems :Arr[X], index :Int, elem :U) :ArrayLike[U] = {
		val res = RefArray.copyOf[U](elems)
		res(index) = elem
		res
	}

	final override def prependedAll[A >: X, O](elems :Arr[X], prefix :O)(implicit likeCollection :LikeCollection[A, O]) = {
		val size = likeCollection.knownSize(prefix)
		val length = elems.length
		if (size >= 0) {
			val res = RefArray.ofDim[A](length + size)
			likeCollection.toIterableOnceOps(prefix).copyToArray(res.asAnyArray)
			ArrayLike.copy(elems, 0, res, size, length)
			res
		} else {
			val array = likeCollection.toRefArray(prefix)
			RefArray.copyOfRanges(array, 0, array.length, elems, 0, length)
		}
	}
	final override def concat[A >: X, O](elems :Arr[X], suffix :O)(implicit likeCollection :LikeCollection[A, O]) = {
		val size   = likeCollection.knownSize(suffix)
		val length = elems.length
		if (size >= 0) {
			val res = RefArray.ofDim[A](length + size)
			ArrayLike.copy(elems, 0, res, 0, length)
			likeCollection.toIterableOnceOps(suffix).copyToArray(res.asAnyArray, length)
			res
		} else {
			val array = likeCollection.toRefArray(suffix)
			RefArray.copyOfRanges(elems, 0, length, array, 0, array.length)
		}
	}
	final override def updatedAll[U >: X, O](elems :Arr[X], index :Int, patch :O)
	                                        (implicit likeCollection :LikeCollection[U, O]) :ArrayLike[U] =
	{
		val thisLength = elems.length
		def oob() = outOfBounds_!(
			toString + ".updatedAll(" + infoString(elems) + ", " + index + ", " + likeCollection.infoString(patch) + ")"
		)
		likeCollection match {
			case _ if index < 0 | index > elems.length           => oob()
			case _ :LikeSeqForArrayLike[U, ArrayLike] @unchecked =>
				val that = patch.asInstanceOf[ArrayLike[U]]
				val thatLength = that.length
				if (index > thisLength - thatLength)
					oob()
				RefArray.copyOfRanges[U](elems, 0, index, that, 0, thatLength, elems, index + thatLength, elems.length)
			case _ => likeCollection.knownSize(patch) match {
				case  0 => copy(elems)
				case -1 =>
					val res = RefArray.from[U](elems)
					val it  = likeCollection.iterator(patch)
					var i   = index
					while (i < thisLength && it.hasNext) {
						res(i) = it.next()
						i += 1
					}
					if (it.hasNext)
						oob()
					res
				case  n if n > thisLength - index => oob()
				case _ =>
					val res = RefArray.from[U](elems)
					likeCollection.copyToArray[Any](patch, res.asAnyArray, index)
					res
			}
		}
	}
	final override def overwritten[U >: X, O](elems :Arr[X], index :Int, patch :O)
	                                         (implicit likeCollection :LikeCollection[U, O]) :ArrayLike[U] =
		if (index == Int.MinValue || index >= elems.length)
			copy(elems)
		else if (index < 0)
			overwritten[U, Iterator[U]](elems, 0, likeCollection.iterator(patch).dropInPlace(-index))
		else {
			val res = RefArray.from[U](elems)
			likeCollection.copyToArray[Any](patch, res.asAnyArray, index, Int.MaxValue)
			res
		}

	final override def patch[U >: X, O](elems :Arr[X], from :Int, other :O, replaced :Int)
	                                   (implicit likeCollection :LikeCollection[U, O]) :ArrayLike[U] =
	{
		val thisLength = elems.length
		val from0 = math.max(0, math.min(thisLength, from))
		val replaced0 = math.max(0, math.min(thisLength - from0, replaced))
		likeCollection.knownSize(other) match {
			case  0 =>
				if (replaced <= 0 || from >= thisLength) copy(elems)
				else if (from <= 0 && replaced >= thisLength) empty(elems)
				else elems.removed(math.max(0, from), replaced)
			case -1 =>
				val res = Array.newBuilder[Any]
				res.addAll(elems.asInstanceOf[Array[X]], 0, from0)
				likeCollection.addTo(other, res)
				res.addAll(elems.asInstanceOf[Array[X]], from0 + replaced0, thisLength - from0 - replaced0)
				res.result().asInstanceOf[IRefArray[U]]
			case thatLength => likeCollection match {
				case _ :LikeSeqForArrayLike[U, ArrayLike] @unchecked =>
					IRefArray.copyOfRanges(
						elems, 0, from0,
						other.asInstanceOf[ArrayLike[U]], 0, thatLength,
						elems, from0 + replaced0, thisLength
					)
				case _ =>
					val res = RefArray.ofDim[U](thisLength - replaced0 + thatLength)
					ArrayLike.copy(elems, 0, res, 0, from0)
					likeCollection.copyToArray[Any](other, res.asAnyArray, from0, thatLength)
					ArrayLike.copy(elems, from0 + replaced0, res, from0 + thatLength, thisLength - from0 - replaced0)
					res
			}
		}
	}

	final override def padTo[A >: X](elems :Arr[X], len :Int, elem :A) = {
		if (elems.length >= len)
			RefArray.copyOf(elems)
		else {
			val res = RefArray.copyOf[A](elems, len)
			res.fill(elems.length, len)(elem)
			res
		}
	}

	override def groupMap[K, A](elems :Arr[X])(key :X => K)(f :X => A) :Map[K, ArrayLike[A]] =
		if (elems.length == 0)
			Map.empty
		else {
			val res = mutable.Map.empty[K, Builder[A, RefArray[A]]]
			val length = elems.length
			var i = 0
			while (i < length) {
				val x = elems(i)
				val k = key(x)
				val v = f(x)
				val b = res.getOrElseUpdate(k, RefArray.newBuilder[A])
				b += v
				i += 1
			}
			Map from res.iterator.map { case (key, builder) => (key, builder.result()) }
		}

	final override def groupBy[K](elems :Arr[X])(f: X => K) :Map[K, Arr[X]] = elems.groupBy(f)


	final override def insertInto[U >: X](elems :Arr[X], buffer :Buffer[U], index :Int) :Unit =
		buffer.insertAll(index, toIterableOnce(elems))

	final override def patchOver[U >: X](elems :Arr[X], buffer :Buffer[U], index :Int, replaced :Int) :Unit =
		buffer.patchInPlace(index, toIterableOnce(elems), replaced)

	final override def copyToArray[A >: X](elems :Arr[X], array :Array[A], start :Int, max :Int) :Int =
		elems.copyToArray(array, start, max)

	final override def withFilter(elems :Arr[X])(p :X => Boolean) :WithFilter[X, ArrayLike] = elems.withFilter(p)

	final override def iterator(elems :Arr[X]) :Iterator[X] = elems.iterator
	final override def reverseIterator(elems :Arr[X]) :Iterator[X] = elems.reverseIterator
	final override def stepper[S <: Stepper[_]](elems :Arr[X])(implicit shape :StepperShape[X, S]) :S = elems.stepper

	final override def view(elems :Arr[X]) :IndexedSeqView[X] = elems.view //new IndexedSeqView.Id(toOps(elems))
	final override def toGeneric[U >: X](elems :Arr[X]) :ArrayLike[U] = elems
	final override def toSpecific(elems :Arr[X]) :Arr[X] = elems
	final override def toImpureSeq(elems :Arr[X]) :collection.Seq[X] = ArrayLike.Wrapped(elems)
	final override def toIterableOnce(elems :Arr[X]) :IterableOnce[X] = elems.iterator
	final override def toIterableOnceOps(elems :Arr[X]) :IterableOnceOps[X, kinds.Any1, Any] = elems.iterator
	final override def toArray[A >: X :ClassTag](elems :Arr[X]) :Array[A] = elems.toArray
	final override def to[C1](elems :Arr[X])(factory :Factory[X, C1]) :C1 =
		factory.fromSpecific(ArrayLike.Wrapped(elems))

	override def infoString(elems :Arr[X]) :String = util.errorString(elems)
}


private class LikeSeqForRefArrayLike[X, Arr[A] <: RefArrayLike[A]] extends LikeSeqForArrayLike[X, Arr] {
	final override def knownSize(elems :Arr[X]) :Int = elems.length
	final override def size(elems :Arr[X]) :Int = elems.length
	final override def apply(elems :Arr[X], i :Int) = elems(i)
}






/** @define coll mutable indexed sequence
  */
trait LikeMutableIndexedSeq[X, -Xs, +CC[_], +C] extends LikeMutableSeq[X, Xs, CC, C] with LikeIndexedSeq[X, Xs, CC, C] {
	def updateAll[O](self :Xs, index :Int, elems :O)(implicit likeCollection :LikeCollection[X, O]) :Int
	def overwrite[O](self :Xs, index :Int, elems :O)(implicit likeCollection :LikeCollection[X, O]) :Int
}




/** @define TypeClass `LikeMutableIndexedSeq` */
private[collections] sealed abstract class Rank2LikeMutableIndexedSeqs
	extends LikeIterableOnceSummons[LikeMutableIndexedSeq]
{
	@inline implicit final def likeGeneric[X, Xs <: CC[X], CC[_], C >: CC[X]]
	                                      (implicit generic :Generic[CC]) :LikeMutableIndexedSeq[X, Xs, CC, C] =
		generic.of
}


private[collections] sealed abstract class Rank1LikeMutableIndexedSeqs extends Rank2LikeMutableIndexedSeqs {
	implicit final def forOps[E, Xs, CC[X], C <: AnyRef]
	                         (implicit arg :Xs <:< C, specific :C <:< CC[E] with mutable.IndexedSeqOps[E, CC, C],
	                                   generic :CC <::< Iterable)
			:LikeMutableIndexedSeq[E, Xs, CC, C] =
		forOpsPrototype.asInstanceOf[LikeMutableIndexedSeq[E, Xs, CC, C]]

	private[this] val forOpsPrototype = new LikeMutableIndexedSeq.ForOps[Any, Iterable, mutable.IndexedSeq[Any]] {
		private def readResolve :AnyRef =
			LikeMutableIndexedSeq.forOps[Any, mutable.IndexedSeq[Any], Iterable, mutable.IndexedSeq[Any]]
		override def toString = "LikeMutableIndexedSeq.forOps"
	}
}


@SerialVersionUID(Ver)
object LikeMutableIndexedSeq extends Rank1LikeMutableIndexedSeqs {

	implicit def forRefArray[E] :LikeMutableIndexedSeq[E, RefArray[E], RefArray, RefArray[E]] =
		refArrayPrototype.asInstanceOf[LikeMutableIndexedSeq[E, RefArray[E], RefArray, RefArray[E]]]

	implicit def forArray[E] :LikeMutableIndexedSeq[E, Array[E], RefArray, Array[E]] =
		arrayPrototype.asInstanceOf[LikeMutableIndexedSeq[E, Array[E], RefArray, Array[E]]]

	private[this] val arrayPrototype :LikeMutableIndexedSeq[Unknown, Array[Unknown], ArrayLike, Array[Unknown]] =
		new LikeSeqForArrayLike[Unknown, Array] with ForMutableArray[Unknown, Array] {
			override def update(elems :Array[Unknown], idx :Int, elem :Unknown) :Unit = elems(idx) = elem
			override def toIterable(elems :Array[Unknown]) = ArrayFactory.Wrapped(elems)
			override def toIndexedSeq(elems :Array[Unknown]) = IArray.Wrapped(IArray.from(elems))
			override def toString = "LikeMutableIndexedSeq.forArray"
			private def readResolve :AnyRef = LikeMutableIndexedSeq.forArray
		}
	private[this] val refArrayPrototype :LikeMutableIndexedSeq[Unknown, RefArray[Unknown], ArrayLike, RefArray[Unknown]] =
		new LikeSeqForRefArrayLike[Unknown, RefArray] with ForMutableArray[Unknown, RefArray] {
			override def update(elems :RefArray[Unknown], idx :Int, elem :Unknown) :Unit = elems(idx) = elem
			override def toIterable(elems :RefArray[Unknown]) = RefArray.Wrapped(elems)
			override def toIndexedSeq(elems :RefArray[Unknown]) = IRefArray.Wrapped(IRefArray.copyOf(elems))
			override def infoString(elems :RefArray[Unknown]) :String = "RefArray|" + elems.length + "|"
			override def toString = "LikeMutableIndexedSeq.forRefArray"
			private def readResolve :AnyRef = LikeMutableIndexedSeq.forRefArray
		}


	def adapt[X, Xs](elems :Xs)(implicit likeSeq :LikeMutableIndexedSeq[X, Xs, Any1, Any]) :mutable.IndexedSeq[X] =
		new LikeCollectionAdapter[X, elems.type](elems)
			with LikeIterableAdapter[X, elems.type, mutable.IndexedSeq, mutable.IndexedSeq[X]] with mutable.IndexedSeq[X]
			with LikeSeqAdapter[X, elems.type, mutable.IndexedSeq, mutable.IndexedSeq[X]]
		{
			override val ops = likeSeq.specific(elems)
			override def update(idx :Int, elem :X) :Unit = likeSeq.update(elems, idx, elem)
			override def iterator :Iterator[X] = ops.iterator(elems)
		}


	trait Generic[CC[_]]
		extends LikeMutableSeq.Generic[CC] with LikeIndexedSeq.Generic[CC] with Template[CC, LikeMutableIndexedSeq]

	@SerialVersionUID(Ver)
	object Generic extends Rank1Generics {
		implicit def forRefArray :Generic[RefArray] = refArray
		//Can't have Generic[Array] because mapping of an array requires a ClassTag.
//		implicit def forArray :Generic[Array] = array

		@SerialVersionUID(Ver)
		private[this] object refArray extends Generic[RefArray] {
			implicit override def of[X] :LikeMutableIndexedSeq[X, RefArray[X], RefArray, RefArray[X]] =
				LikeMutableIndexedSeq.forRefArray
			override def toString = "LikeMutableIndexedSeq.Generic.forRefArray"
		}
//		@SerialVersionUID(Ver)
//		private[this] object array extends Generic[Array] {
//			implicit override def of[X] :LikeMutableIndexedSeq[X, Array[X], Array, Array[X]] =
//				LikeMutableIndexedSeq.forArray
//			override def toString = "LikeMutableIndexedSeq.Generic.forRefArray"
//		}
	}

	private[LikeMutableIndexedSeq] sealed abstract class Rank1Generics {
		implicit final def forOps[CC[X] <: Iterable[X] with mutable.IndexedSeqOps[X, CC, CC[X]]] :Generic[CC] =
			prototype.asInstanceOf[Generic[CC]]

		private[this] val prototype =
			new SerializableSingleton("LikeMutableIndexedSeq.Generic.forOps", Generic.forOps[mutable.IndexedSeq])
				with Generic[mutable.IndexedSeq]
			{
				implicit override def of[X] :LikeMutableIndexedSeq[X, mutable.IndexedSeq[X], mutable.IndexedSeq, mutable.IndexedSeq[X]] =
					LikeMutableIndexedSeq.forOps
			}
	}



	trait FromOps[X, -Xs, +CC[_], +C <: AnyRef]
		extends LikeMutableIndexedSeq[X, Xs, CC, C]
		   with LikeMutableSeq.FromOps[X, Xs, CC, C] with LikeIndexedSeq.FromOps[X, Xs, CC, C]
	{
		override def updateAll[O](self :Xs, index :Int, elems :O)(implicit likeCollection :LikeCollection[X, O]) :Int =
			likeCollection match {
				case _ :LikeCollection.ForIterableOnce[X, IterableOnce[X]] @unchecked =>
					toOps(self).updateAll(index, likeCollection.toIterableOnce(elems))
				case _ if likeCollection.knownSize(elems) == 0 =>
					0
				case _ =>
					toOps(self).updateAll(index, likeCollection.toIterableOnce(elems))
			}
		override def overwrite[O](self :Xs, index :Int, elems :O)(implicit likeCollection :LikeCollection[X, O]) :Int =
			likeCollection match {
				case _ :LikeCollection.ForIterableOnce[X, IterableOnce[X]] @unchecked =>
					toOps(self).overwrite(index, likeCollection.toIterableOnce(elems))
				case _ if index == Int.MinValue || index >= size(self) =>
					0
				case _ =>
					val size = likeCollection.knownSize(elems)
					if (size == 0 | index < 0 & size >= 0 & size <= -index)
						0
					else
						toOps(self).overwrite(index, likeCollection.toIterableOnce(elems))
			}
		override def toOps(elems :Xs) :mutable.IndexedSeqOps[X, CC, C]
	}


	trait ForOps[X, CC[x] <: Iterable[x], C <: CC[X] with mutable.IndexedSeqOps[X, CC, C]]
		extends FromOps[X, C, CC, C]
		   with LikeMutableSeq.ForOps[X, CC, C] with LikeIndexedSeq.ForOps[X, CC, C]
	{
		override def toOps(elems :C) :mutable.IndexedSeqOps[X, CC, C] = elems
	}


	trait FromIterator[X, -Xs, +CC[_], +C <: AnyRef]
		extends LikeMutableSeq.FromIterator[X, Xs, CC, C] with LikeIndexedSeq.FromIterator[X, Xs, CC, C]
		   with LikeMutableIndexedSeq[X, Xs, CC, C]



	private trait ForMutableArray[X, Arr[x] <: MutableArray[x]]
		extends LikeSeqForArrayLike[X, Arr] with LikeMutableIndexedSeq[X, Arr[X], ArrayLike, Arr[X]]
	{
		override def updateAll[O](self :Arr[X], index :Int, elems :O)
		                         (implicit likeCollection :LikeCollection[X, O]) :Int =
			likeCollection match {
				case _ :LikeSeqForArrayLike[X, Arr] @unchecked =>
					self.updateAll(index, elems.asInstanceOf[ArrayLike[X]])
				case _ => likeCollection.knownSize(elems) match {
					case  0 => 0
					case -1 => self.updateAll(index, likeCollection.toIterableOnce(elems))
					case  n if index < 0 | index > self.length - n => outOfBounds_!(
						toString + ".updateAll(" + infoString(self) + ", " + index + ", " +
							likeCollection.infoString(elems) + ")"
					)
					case  _ => likeCollection.copyToArray(elems, self.asInstanceOf[Array[X]], index, Int.MaxValue)
				}
			}
		override def overwrite[O](self :Arr[X], index :Int, elems :O)
		                         (implicit likeCollection :LikeCollection[X, O]) :Int =
			likeCollection match {
				case _ :LikeSeqForArrayLike[X, Arr] @unchecked =>
					self.overwrite(index, elems.asInstanceOf[ArrayLike[X]])
				case _ :LikeCollection.ForIterableOnce[X, IterableOnce[X]] @unchecked =>
					self.overwrite(index, likeCollection.toIterableOnce(elems))
				case _ if index == Int.MinValue || index >= self.length =>
					0
				case _ =>
					val size = likeCollection.knownSize(elems)
					if (size == 0 | size >= 0 & index <= 0 & size <= -index)
						0
					else
						self.overwrite(index, likeCollection.toIterableOnce(elems))
			}
	}
}
