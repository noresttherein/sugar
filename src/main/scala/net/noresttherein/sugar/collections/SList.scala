package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{BufferedIterator, IterableFactoryDefaults, SeqFactory, StrictOptimizedLinearSeqOps, StrictOptimizedSeqFactory}
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSeq, LinearSeq, LinearSeqOps}
import scala.collection.mutable.Builder

import net.noresttherein.sugar.collections.Iterators.IteratorTake
import net.noresttherein.sugar.collections.SList.{Link, SListIterator}
import net.noresttherein.sugar.concurrent.Fences.releaseFence
import net.noresttherein.sugar.exceptions.{??!, noSuch_!, unsupported_!}




/** An immutable linked list of precomputed length.
  * The single empty instance is [[net.noresttherein.sugar.collections.SNil SNil]], and can be used for pattern matching.
  * Non-empty lists may be deconstructed using the standard Scala `+:` object:
  * {{{
  *     list match {
  *         case first +: second +: SNil =>
  *     }
  * }}}
  * When using a builder to create a new instance, provide a size hint, if known,
  * to avoid traversing the whole list to set length for every element.
  * @define Coll `SList`
  * @define coll sized list
  */
@SerialVersionUID(Ver)
sealed abstract class SList[+E]
	extends AbstractSeq[E] with LinearSeq[E] with LinearSeqOps[E, SList, SList[E]]
	   with StrictOptimizedLinearSeqOps[E, SList, SList[E]]
	   with SugaredIterable[E] with SugaredSeqOps[E, SList, SList[E]]
	   with IterableFactoryDefaults[E, SList] with DefaultSerializable
{
	override def knownSize :Int = length
	final override def isEmpty :Boolean = this eq SNil

	override def prepended[U >: E](elem :U) :SList[U] = {
		val res = new Link(elem, this, length + 1)
		releaseFence()
		res
	}
	override def prependedAll[U >: E](elems :IterableOnce[U]) :SList[U] = elems match {
		case _ if elems.knownSize == 0 =>
			this
		case indexed :collection.IndexedSeq[U] =>
			@tailrec def prependIndexed(idx :Int, res :SList[U], size :Int) :SList[U] =
				if (idx >= 0)
					prependIndexed(idx - 1, new Link(indexed(idx), res, size + 1), size + 1)
				else {
					releaseFence(); res
				}
			prependIndexed(indexed.length - 1, this, length)

		case sized if sized.knownSize >= 0 => prependedAll(DefaultBuffer from sized)

		case list :collection.LinearSeq[U] =>
			@tailrec def prependReversed(reverse :collection.LinearSeq[U], res :SList[U], size :Int) :SList[U] =
				if (reverse.isEmpty) {
					releaseFence(); res
				} else
					prependReversed(reverse.tail, new Link(reverse.head, res, size + 1), size + 1)
			prependReversed(list.reverse, this, length)
		case _ =>
			super.prependedAll(elems)
	}

	override def appended[U >: E](elem :U) :SList[U] = this ++: elem +: SNil
	override def appendedAll[U >: E](elems :IterableOnce[U]) :SList[U] = elems match {
		case _ if elems.knownSize == 0 => this
		case other :SList[U]           => this ++: other
		case _ if this eq SNil         => SList from elems
		case _                         => super.appendedAll(elems)
	}

	final override def reverse :SList[E] = {
		@tailrec def rec(list :SList[E], res :SList[E], size :Int) :SList[E] = list match {
			case link :Link[E] => rec(link.tail, new Link(link.head, res, size + 1), size + 1)
			case _             => releaseFence(); res
		}
		rec(this, SNil, 0)
	}

	final override def drop(n :Int) :SList[E] = {
		@tailrec def drop(list :SList[E], left :Int) :SList[E] = list match {
			case _ if left <= 0 => list
			case link :Link[E]  => drop(link.tail, left - 1)
			case _              => list
		}
		if (n >= length) SNil else drop(this, n)
	}
	final override def dropWhile(p :E => Boolean) :SList[E] = {
		@tailrec def drop(list :SList[E]) :SList[E] = list match {
			case link :Link[E] => if (p(link.head)) drop(link.tail) else list
			case _             => list
		}
		drop(this)
	}
	final override def splitAt(n :Int) :(SList[E], SList[E]) =
		if (n <= 0)
			(SNil, this)
		else if (n >= length)
			(this, SNil)
		else {
			val b = SList.newBuilder[E](n)
			@tailrec def split(list :SList[E], left :Int) :(SList[E], SList[E]) = list match {
				case _ if left <= 0 => (b.result(), list)
				case link :SList[E] => b += link.head; split(link.tail, left - 1)
				case _              => ??!
			}
			split(this, n)
		}
	final override def span(p :E => Boolean) :(SList[E], SList[E]) = {
		val prefix = SList.newBuilder[E]
		@tailrec def split(suffix :SList[E]) :(SList[E], SList[E]) = suffix match {
			case link :Link[E] =>
				val next = link.head
				if (p(next)) {
					prefix += next
					split(link.tail)
				} else
					(prefix.result(), suffix)
			case _ => (this, SNil)
		}
		split(this)
	}

	final override def foreach[U](f :E => U) :Unit = {
		@tailrec def loop(list :SList[E]) :Unit = list match {
			case link :Link[E] => f(link.head); loop(link.tail)
			case _ =>
		}
		loop(this)
	}
	final override def foldLeft[A](z :A)(op :(A, E) => A) :A = {
		@tailrec def fold(list :SList[E], acc :A) :A = list match {
			case link :Link[E] => fold(link.tail, op(acc, link.head))
			case _             => acc
		}
		fold(this, z)
	}
	final override def reduceLeft[A >: E](op :(A, E) => A) :A = this match {
		case link :Link[E] =>
			@tailrec def reduce(list :SList[E], acc :A) :A = list match {
				case link :Link[E] => reduce(link.tail, op(acc, link.head))
				case _             => acc
			}
			reduce(link.tail, link.head)
		case _ =>
			unsupported_!("SList.empty.reduceLeft")
	}

	final override def find(p :E => Boolean) :Option[E] = {
		@tailrec def loop(list :SList[E]) :Option[E] = list match {
			case link :Link[E] =>
				val next = link.head
				if (p(next)) Some(next) else loop(link.tail)
			case _ => None
		}
		loop(this)
	}

	private def indexWhere(p :E => Boolean, from :Int, flipped :Boolean) :Int =
		//Sadly, Vector and List return segmentLength(-1, - => true) == length, not 0
//		if (from >= length | flipped & from < 0)
		if (from >= length)
			-1
		else drop(from) match {
			case link :Link[E] =>
				var list = link
				var i = math.max(from, 0)
				while (p(list.head) == flipped && (list.tail match {
					case link :Link[E] => list = link; i += 1; true
					case _ => list = null; false
				})) {}
				if (list eq null) -1 else i
			case SNil => -1
		}
	final override def indexWhere(p :E => Boolean, from :Int) :Int = indexWhere(p, from, false)
	final override def segmentLength(p :E => Boolean, from :Int) :Int = indexWhere(p, from, true) match {
		case -1 => math.max(0, length - math.max(0, from))
		case  n => n - math.max(0, from)
	}
	final override def forall(p :E => Boolean) :Boolean = indexWhere(p, 0, true) == -1
	final override def exists(p :E => Boolean) :Boolean = indexWhere(p, 0, false) != -1

	override def iterator :Iterator[E] = if (this eq SNil) Iterator.empty else new SListIterator(this, length)

	override def iterableFactory :SeqFactory[SList] = SList
	protected[this] override def className :String = "SList"
}




/** $factoryInfo
  * @define Coll `SList`
  * @define coll sized list
  */
@SerialVersionUID(Ver)
case object SList extends StrictOptimizedSeqFactory[SList] {
	override def from[E](source :IterableOnce[E]) :SList[E] = source match {
		case list :SList[E]             => list
		case _ if source.knownSize == 0 => SNil
		case _ => (newBuilder[E](source.knownSize) ++= source).result()
	}

	override def empty[E] :SList[E] = SNil

	override def newBuilder[E] :Builder[E, SList[E]] = new SListBuilder(0)
	def newBuilder[E](sizeHint :Int) :Builder[E, SList[E]] = new SListBuilder(sizeHint)

	private sealed class Link[+E](final override val head :E, private[SList] final var next :SList[E @uncheckedVariance],
	                              private[SList] final var len :Int)
		extends SList[E]
	{
		final override def length = len
		final override def headOption = Some(head)
		final override def tail = next
	}

	/** By extending `Link` we serve as our own dummy pre-head. */
	private final class SListBuilder[E](hint :Int)
		extends Link[E](null.asInstanceOf[E], SNil, hint) with Builder[E, SList[E]]
	{
		private[this] var coccyx :Link[E] = this   //The last link of the built sequence.
		override def knownSize = len - coccyx.len

		override def sizeHint(size :Int) :Unit =
			//Appending an element to coccyx decreases coccyx.len, so after adding size elements,
			//coccyx.len will equal 1, as it should for the last element.
			if (size > 0 & (coccyx eq this))
				len = size + 1

		override def addOne(elem :E) = {
			val next = new Link(elem, SNil, coccyx.len - 1)
			coccyx.next = next
			coccyx = next
			this
		}
		override def result() =
			if (next eq SNil)
				next
			else {
				var link = next.asInstanceOf[Link[E]]
				if (coccyx.len != 1) {          //No size hint was given, or a wrong one - compute lengths.
					var size = len - coccyx.len
					link.len = len - coccyx.len
					var tail = link.tail
					while (tail ne SNil) {
						link = tail.asInstanceOf[Link[E]]
						size -= 1
						link.len = size
						tail = link.tail
					}
					link = next.asInstanceOf[Link[E]]
				}
				assert(coccyx.len == 1)
				clear()
				releaseFence()
				link
			}
		override def clear() :Unit = {
			next  = SNil
			len   = 0
			coccyx = this
		}
	}

	private class SListIterator[+E](private[this] var list :SList[E], countDown :Int)
		extends IteratorTake[E](countDown) with BufferedIterator[E]
	{
		override def head = list.head
		override def headOption = list.headOption
		override def next() :E = list match {
			case _ if !hasNext => noSuch_!("SList.iterator.next")
			case link :Link[E] => knownSize_--(); list = link.tail; link.head
			case _             => ??!
		}

		override def foreach[U](f :E => U) :Unit = list.foreach(f)
		override def foldLeft[B](z :B)(op :(B, E) => B) :B = list.foldLeft(z)(op)
		override def reduceLeft[A >: E](op :(A, E) => A) :A = list.reduceLeft(op)
		override def reduceLeftOption[A >: E](op :(A, E) => A) :Option[A] = list.reduceLeftOption(op)

		override def strictDrop(n :Int) = {
			if (n > 0) {
				val size = knownSize
				if (n >= size) {
					list = SNil
					knownSize = 0
				} else {
					list = list.drop(n)
					knownSize = size - n
				}
			}
			this
		}
		//This is eager, which is not what List.iterator does.
//		override def dropWhile(p :E => Boolean) = {
//			if (knownSize == list.length) {
//				list = list.dropWhile(p)
//				knownSize = list.length
//			} else
//				super.dropWhile(p)
//			this
//		}

	}

}




@SerialVersionUID(Ver)
case object SNil extends SList[Nothing] {
	override def length = 0
	override def head :Nothing = noSuch_!("SList.empty.head")
	override def tail :Nothing = unsupported_!("SList.empty.tail")
}

