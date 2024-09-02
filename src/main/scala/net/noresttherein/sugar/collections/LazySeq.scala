package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{AbstractIterable, Factory, IterableOps, SeqFactory}
import scala.collection.immutable.{LinearSeq, LinearSeqOps, SeqOps}
import scala.collection.mutable.{Builder, ReusableBuilder}

import net.noresttherein.sugar.collections.LazyLinearSeq.Link
import net.noresttherein.sugar.collections.extensions.FactoryExtension
import net.noresttherein.sugar.concurrent.Fences.{acquireFence, releaseFence}
import net.noresttherein.sugar.exceptions.outOfBounds_!
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Term.{Expression, Value}
import net.noresttherein.sugar.vars.{Maybe, Term}




/** Lazy equivalents of `SeqOps` methods building `Coll` instances.
  * @tparam E     $E
  * @tparam CC    $CC
  * @tparam C     $C
  * @tparam LCC   $LCC
  * @tparam LC    $LC
  * @define Coll  `LazySeq`
  * @define coll  lazy sequence
  * @define Eager `Seq`
  */
sealed trait LazySeqOps[+E, +CC[+X] <: Seq[X] with SeqOps[X, CC, CC[X]], +C <: CC[E] with SeqOps[E, CC, C],
                        +LCC[+X] <: CC[X] with SeqOps[X, LCC, LCC[X]], +LC <: LCC[E] with SeqOps[E, LCC, LC]]
	extends LazyIterableOps[E, CC, C, LCC, LC] with SeqOps[E, LCC, LC]
{
	override def length :Int = definite.length

	override def apply(i :Int) :E = definite(i)

	@inline final override def addedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyAppendedAll(elems)
	@inline final override def added[U >: E](elem: => U) :LCC[U] = lazyAppended(elem)

	override def appendedAll[U >: E](elems :IterableOnce[U]) :LCC[U] = lazyGeneric(definite appendedAll elems)
	override def prependedAll[U >: E](elems :IterableOnce[U]) :LCC[U] = lazyGeneric(definite prependedAll elems)

	def appendedAll[U >: E](elems :() => IterableOnce[U]) :LCC[U] = lazyAppendedAll(elems())
	def prependedAll[U >: E](elems :() => IterableOnce[U]) :LCC[U] = lazyPrependedAll(elems())

	@inline final def :++[U >: E](elems :() => IterableOnce[U]) :LCC[U] = appendedAll(elems)
	@inline final def ++:[U >: E](elems :() => IterableOnce[U]) :LCC[U] = prependedAll(elems)

	override def appended[U >: E](elem :U) :LCC[U] = lazyGeneric(definite appended elem)
	//consider: always creating a LazySeqLink on prepend/lazyPrepend
	override def prepended[U >: E](elem :U) :LCC[U] = lazyGeneric(definite prepended elem)

	def lazyAppended[U >: E](elem: => U) :LCC[U] = lazyFactory.from(() => appended(elem) :CC[U])
	def lazyPrepended[U >: E](elem: => U) :LCC[U] = lazyFactory.from(() => prepended(elem))

	def lazyAppendedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyFactory.from(() => appendedAll(elems))
	def lazyPrependedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyFactory.from(() => prependedAll(elems))

	@inline final def :+#[U >: E](elem: => U) :LCC[U] = lazyAppended(elem)
	@inline final def #+:[U >: E](elem: => U) :LCC[U] = lazyPrepended(elem)

	@inline final def :++#[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyAppendedAll(elems)
	@inline final def #++:[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyPrependedAll(elems)

	def reversePrependedAll[U >: E](elems :IterableOnce[U]) :LCC[U] = lazyGeneric {
		val itr = elems.iterator
		var res = definite :CC[U]
		while (itr.hasNext)
			res = itr.next() +: res
		res
	}
	def reversePrependedAll[U >: E](elems :() => IterableOnce[U]) :LCC[U] = lazyReversePrependedAll(elems())

	def lazyReversePrependedAll[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyGeneric {
		val itr = elems.iterator
		var res = definite :CC[U]
		while (itr.hasNext)
			res = itr.next() +: res
		res
	}

	@inline final def reverse_++:[U >: E](elems: IterableOnce[U]) :LCC[U] = reversePrependedAll(elems)
	@inline final def reverse_#++:[U >: E](elems: => IterableOnce[U]) :LCC[U] = lazyReversePrependedAll(elems)

	override def iterator :Iterator[E] = super[LazyIterableOps].iterator
}




/** A base class of sequences with lazily evaluated contents. This includes both those built recursively
  * like a `LazyList`, and simple wrappers over lazy expressions evaluating to a `Seq` or its subtype.
  * @tparam E     $E
  * @define Coll  `LazySeq`
  * @define coll  lazy sequence
  * @define Eager `Seq`
  */
abstract class LazySeq[+E] //Consider: extending SugaredIterable with SugaredSeqOps
	extends AbstractIterable[E] with LazyIterable[E] with LazySeqOps[E, Seq, Seq[E], LazySeq, LazySeq[E]]
	   with Seq[E] with SeqOps[E, LazySeq, LazySeq[E]]// with IterableFactoryDefaults[E, LazySeq]
	   with LazyIterableFactoryDefaults[E, Seq, LazySeq]
{
	override def iterableFactory :SeqFactory[LazySeq] = LazySeq.factory

	def toLazyList :LazyList[E] = LazyList.from(this)

	override def to[C1](factory :Factory[E, C1]) :C1 = (factory.source :Any) match {
		case LazyList => toLazyList.asInstanceOf[C1]
		case _        => super.to(factory)
	}
}


/** $lazyFactoryInfo
  * @define Coll  `LazySeq`
  * @define coll  lazy sequence
  * @define Eager `Seq`
  */
@SerialVersionUID(Ver)
case object LazySeq extends DelayedFactory[Seq, LazySeq] {
	override def from[E](items :() => Seq[E]) :LazySeq[E] =
		new Delayed(items)
//		LazyLinearSeq.from(() => LazyLinearSeq.factory.from(items()))

//	override val factory :SeqFactory[LazySeq] = LazyLinearSeq.factory
	object factory extends SeqFactory.Delegate[LazySeq](LazyLinearSeq.factory) {
		override def from[E](it :IterableOnce[E]) :LazySeq[E] = it match {
			case seq :LazySeq[E] => seq
			case _               => LazyLinearSeq.factory.from(it)
		}
		override def toString = "LazySeq.factory"
	}

	def empty[E] :LazySeq[E] = LazyLinearSeq.Empty

	/** Matches lazy linked sequences, splitting them into their first element and the rest.
	  * It will match any [[net.noresttherein.sugar.collections.LazySeq LazySeq]], even those not extending
	  * [[net.noresttherein.sugar.collections.LazyLinearSeq LazyLinearSeq]], but with much lower the performance
	  * in the latter case. Standard 'delayed' instances are split in amortized `O(1)` (when called recursively
	  * for the returned tail sequence).
	  *
	  * Does not match empty sequences and those not extending `LazySeq`.
	  */
	@SerialVersionUID(Ver)
	object #+: {
		def unapply[X](seq :collection.Seq[X]) :Maybe[(Term[X], Term[LazySeq[X]])] = seq match {
			case link :LazySeqLink[X]             => Yes((link.headTerm, link.tailTerm))
			case _ if seq.isEmpty                 => No
			case lzy  :LazySeq[X] if lzy.isStrict => Yes((Value(lzy.head), Value(lzy.tail)))
			case lzy  :LazySeq[X]                 =>
				Yes((Expression(() => lzy.head), Expression(() => lzy.tail)))
			case _                                => No
		}
	}

	@SerialVersionUID(Ver)
	private class Delayed[E](protected[this] override var initializer: () => Seq[E]) extends LazySeq[E] {
		protected override def lazyFactory :DelayedFactory[Seq, LazySeq] = LazySeq

		override def lazyPrepended[U >: E](elem : => U) :LazySeq[U] =
			if (isDefinite)
				LazyLinearSeq.Link(Expression(() => elem), Value(LazyLinearSeq.factory.from(definite)))
			else
				new Delayed(() => elem +: definite)
	}
}




/** A partially or fully lazily evaluated list. Methods `head` and `tail` are guaranteed to be only amortized `O(1)`
  * when called recursively.
  * @tparam E     $E
  * @define Coll  `LazyLinearSeq`
  * @define coll  lazy linear sequence
  * @define Eager `LinearSeq`
  */
abstract class LazyLinearSeq[+E]
	extends LazySeq[E] with LazySeqOps[E, LinearSeq, LinearSeq[E], LazyLinearSeq, LazyLinearSeq[E]]
	   with LinearSeq[E] with LinearSeqOps[E, LazyLinearSeq, LazyLinearSeq[E]]
	   with LazyIterableFactoryDefaults[E, LinearSeq, LazyLinearSeq]
//	   with IterableFactoryDefaults[E, LazyLinearSeq]
{
	override def isEmpty :Boolean = definite.isEmpty //LinearSeq.isEmpty causes stack overflow with lengthCompare
	override def length :Int = super[LinearSeq].length

	override def prepended[U >: E](elem :U) :LazyLinearSeq[U] =
		Link[U](Value(elem), Value(this))

	override def lazyPrepended[U >: E](elem : => U) :LazyLinearSeq[U] =
		Link[U](Expression(() => elem), Value(this))

	protected override def lazyFactory :DelayedFactory[LinearSeq, LazyLinearSeq] = LazyLinearSeq
//	protected override def lazySpecific(items : => LinearSeq[E @uncheckedVariance]) :LazyLinearSeq[E] =
//		LazyLinearSeq.from(() => items)

	override def iterator :Iterator[E] = super[LinearSeqOps].iterator

	override def iterableFactory :SeqFactory[LazyLinearSeq] = LazyLinearSeq.factory

	def definitePrefixString :String = definitePrefixString(", ")
	def definitePrefixString(separator :String) :String =
		definitePrefixString(className + '(', separator, ")")

	def definitePrefixString(prefix :String, separator :String, suffix :String) :String =
		definitePrefixString(prefix, "?", separator, "..." + suffix, suffix)

	/** Formats this sequence without evaluating any lazy components in a manner similar to `mkString`.
	  * @param prefix      The prefix of the returned string.
	  * @param unevaluated A placeholder string used instead of a not evaluated element, in cases where the following
	  *                    sequence suffix is at least partially evaluated. If the length of this sequence
	  *                    can be calculated without evaluating any lazy expressions, or when it is known
	  *                    to be greater than a given number of elements, the returned string will contain
	  *                    a substring in the form of:
	  *                    {{{
	  *                     unevaluated + separator + unevaluated + separator + ... + separator + unevaluated
	  *                    }}}
	  * @param separator   A string inserted into the result between every adjacent element pair.
	  *                    It is completely ignored if this sequence is known to be empty,
	  *                    or neither its first element nor the tail are not evaluated (at least partially).
	  *                    An empty sequence is formatted as `prefix + fullSuffix`.
	  *                    An unevaluated (with no evaluated components) sequence is formatted as `prefix + cutSuffix`.
	  * @param cutSuffix  the suffix string ending the result if its length is not completely evaluated
	  *                   (an unevaluated `tail` is encountered).
	  * @param fullSuffix the suffix string ending the result if its length is completely evaluated
	  *                   (although possibly with non evaluated elements).
	  * @example {{{
	  *     def format(seq :LazyLinearSeq[Int]) = seq.definitePrefixString("(", "?", ", ", "...)", ")")
	  *     format(LazyLinearSeq.empty)               // "()"
	  *     format(LazyLinearSeq(1::2::Nil))          //"(...)"
	  *     format(1 +: LazyLinearSeq(2::Nil))        //"(1, ...)"
	  *     format(1 +: 2 +: LazyLinearSeq.empty)     //"(1, 2)"
	  *     format(1 #+: LazyLinearSeq(2::Nil))       //"(?, ...)"
	  *     format(1 #+: 2 #+: LazyLinearSeq.empty)   //"(?, ?)"
	  * }}}
	  *
	  */
	def definitePrefixString(prefix :String, unevaluated :String, separator :String,
	                         cutSuffix :String, fullSuffix :String) :String =
	{
		val res = new StringBuilder(prefix)
		@tailrec def addString(seq :LazyLinearSeq[E], sep :String) :Unit = seq match {
			case link :LazySeqLink[E] =>
				res append sep
				link.headTerm match {
					case Value(h) => res append h
					case _        => res append unevaluated
				}
				link.tailTerm match {
					case Value(t) => addString(t, separator)
					case _        => res append cutSuffix
				}
			case _ if seq.isEmpty => res append fullSuffix

			case delayed :LazyLinearSeq.Delayed[E] =>
				if (delayed.isDefinite)
					delayed.definite match {
						case lzy :LazyLinearSeq[E] => addString(lzy, sep)
						case seq                   => seq.addString(res, sep, separator, fullSuffix)
					}
				else
					res append sep append cutSuffix
			case _ if seq.isStrict => //Guard cases, doesn't really occur.
				seq.strict.addString(res, sep, separator, fullSuffix)
			case _ =>
				res append sep append cutSuffix
		}
		addString(this, "")
		res.result()
	}

	override def toString :String = definitePrefixString
}


/** $lazyFactoryInfo
  * @define Coll  `LazyLinearSeq`
  * @define coll  lazy linear sequence
  * @define Eager `LinearSeq`
  */
@SerialVersionUID(Ver)
case object LazyLinearSeq extends DelayedFactory[LinearSeq, LazyLinearSeq] {
	override def from[X](items :() => LinearSeq[X]) :LazyLinearSeq[X] = new Delayed(items)

	def empty[X] :LazyLinearSeq[X] = Empty

	/** A factory of lazy linear sequences similar to `LazyList.cons`, allowing to prepend a single lazy element
	  * to a lazy linear sequence. It serves also as a match pattern for a non-empty `LazyList`-like implementation
	  * of [[net.noresttherein.sugar.collections.LazyLinearSeq LazyLinearSeq]].
	  * Instances of this class are created by prepending elements to a lazy sequence
	  * using either [[net.noresttherein.sugar.collections.LazySeq!.#+: #+:]] or `+:` method.
	  * It will ''not'' match completely non evaluated instances created using
	  * [[net.noresttherein.sugar.collections.LazyLinearSeq$ LazyLinearSeq]]`.`[[net.noresttherein.sugar.collections.LazyLinearSeq$.apply LazyLinearSeq.apply]]
	  * and similar methods (including extension method
	  * [[net.noresttherein.sugar.collections.extensions.SeqLazyMethods.delay delay]])
	  */
	object Link {
		@inline def apply[X](head: => X, tail: => LazyLinearSeq[X]) :LinearSeq[X] =
			apply(Expression(() => head), Expression(() => tail))

		def apply[X](head :Term[X], tail :Term[LazyLinearSeq[X]]) :LazyLinearSeq[X] = {
			val res = new LazySeqLink(head, tail)
			releaseFence()
			res
		}

		def unapply[X](seq :collection.Seq[X]) :Maybe[(Term[X], Term[LazyLinearSeq[X]])] = seq match {
			case link :LazySeqLink[X] => acquireFence(); Yes((link.headTerm, link.tailTerm))
			case _                    => No
		}
		object evaluate {
			def unapply[X](seq :collection.Seq[X]) :Maybe[(X, LazyLinearSeq[X])] = seq match {
				case link :LazySeqLink[X] => Yes((link.head, link.tail))
				case _                    => No
			}
		}
	}

	/** $factoryInfo
	  * @define Coll `LazyLinearSeq`
	  * @define coll lazy linear sequence
	  */
	override object factory extends SeqFactory[LazyLinearSeq] {
		private def slicingLazyLinearSeq[A](source :IterableOps[A, Iterable, Iterable[A]]) :LazyLinearSeq[A] =
			if (source.isEmpty) Empty
			else Link(Expression(() => source.head), Expression(() => slicingLazyLinearSeq(source.tail)))

		private def indexedToLazyLinearSeq[A](seq :collection.IndexedSeq[A], len :Int, offset :Int) :LazyLinearSeq[A] =
			if (offset == len)
				Empty
			else
				Link(Value(seq(offset)), Expression(() => indexedToLazyLinearSeq(seq, len, offset + 1)))

		private def iteratorToLazyLinearSeq[A](itr :Iterator[A]) :LazyLinearSeq[A] = {
			val hd = () => itr.next()
			val tl = new (() => LazyLinearSeq[A]) { //Not a lambda because we use this reference inside apply.
				override def apply() :LazyLinearSeq[A] =
					if (!itr.hasNext) Empty else iteratorToLazyLinearSeq(itr, hd, this)
			}
			iteratorToLazyLinearSeq(itr, hd, tl)
		}
		private def iteratorToLazyLinearSeq[A](itr :Iterator[A], hd :() => A, tl :() => LazyLinearSeq[A]) :LazyLinearSeq[A] =
			if (!itr.hasNext)
				Empty
			else {
				val hd = () => itr.next()
				val res = new LazySeqLink(Expression(hd), null.asInstanceOf[Term[LazyLinearSeq[A]]])
				res.tailTerm = Expression { res.head; tl }
				releaseFence()
				res
			}


		override def from[A](source :IterableOnce[A]) :LazyLinearSeq[A] = source match {
			case lzy  :LazyLinearSeq[A]         => lzy
			case list :LinearSeq[A]             => new Delayed(() => list)
//			case seq  :collection.IndexedSeq[A] => indexedToLazyLinearSeq(seq, seq.length, 0)
			//This is a memory leak, but so, in general, is the following iterator case.
			case HasFastSlice(seq)              => slicingLazyLinearSeq(seq)
			case _                              => iteratorToLazyLinearSeq(source.iterator)
		}

		override def empty[A] :LazyLinearSeq[A] = Empty

		override def newBuilder[A] :Builder[A, LazyLinearSeq[A]] = new LazyLinearSeqBuilder[A]

		override def toString = "LazyLinearSeq.factory"
	}




	/** An empty (only nominally) lazy sequence. This is '''not''' the only empty instance of `LazyLinearSeq`.
	  * However, it will equal any other empty sequence, so may be used in pattern matching as a sequence terminator.
	  * Elements are typically prepended with `lazyExpr `[[net.noresttherein.sugar.collections.LazySeqOps.#+: #+:]]` Empty`.
	  * A companion `#+:` [[net.noresttherein.sugar.collections.LazySeq$.#+: object]] allows to perform
	  * infix pattern matching similarly to the standard scala `+:` object:
	  * {{{
	  *     seq match {
	  *         case term #+: Empty => ...
	  *         case _ => ...
	  *     }
	  * }}}
	  */
	@SerialVersionUID(Ver)
	object Empty extends LazyLinearSeq[Nothing] {
		protected[this] override var initializer :() => LinearSeq[Nothing] = () => Nil
		definite //evaluate the value.
		override def isEmpty :Boolean = true
		override def toLazyList :LazyList[Nothing] = LazyList.empty
		override def toString = "LazyLinearSeq()"
	}


	@SerialVersionUID(Ver)
	private class Delayed[E](protected var initializer: () => LinearSeq[E]) extends LazyLinearSeq[E] with Serializable

	/** Eagerly builds a `LazyLinearSeq` consisting of `LazySeqLink` elements in a manner similar to `ListBuffer`.
	  * It introduces two optimizations:
	  *   1. it attempts to reuse every appended non-empty `LazyLinearSeq` as the suffix
	  *      of the returned sequence, copying the contents only if more elements are appended, and
	  *   2. it extends `LazySeqLink` itself, which allows to both use it as its own 'dummy head' link
	  *      and maintains the invariant that `this.tail` is the returned instance without the need for checking
	  *      for an empty buffer.
	  */
	private class LazyLinearSeqBuilder[E]
		extends LazySeqLink[E](Expression.nothing, Value(Empty))
		   with ReusableBuilder[E, LazyLinearSeq[E]] with BaseGrowable[E]
	{
		private[this] var end :LazySeqLink[E] = this
		private[this] val nil = Value(Empty) //reusable constant

		override def knownSize = if (end eq this) 0 else -1
		override def clear() :Unit = { tailTerm = nil; end = this }
		override def result() = { val res = tail; clear(); releaseFence(); res }

		override def addOne(elem :E) :this.type = {
			var coccyx = end.tail //May be shared with other threads, so must be handled in a thread safe fashion.
			while(!coccyx.isEmpty) {
				val head = coccyx match {
					case link :LazySeqLink[E] => link.dependentHead
					case seq  :Delayed[E]     => Value(seq.head)
					case seq                  => Expression(() => seq.head)
				}
				val next = new LazySeqLink(head, nil)
				end.tailTerm = Value(next)
				end = next
				coccyx = coccyx.tail
			}
			val next = new LazySeqLink(Value(elem), nil)
			end.tailTerm = Value(next)
			end = next
			this
		}

		override def addAll(elems :IterableOnce[E]) :this.type = elems match {
			case lzy  :LazyLinearSeq[E]          => end.tailTerm = Value(lzy); this
			case it   :Iterable[E] if it.isEmpty => this
			case list :LinearSeq[E]              => end.tailTerm = Value(LazyLinearSeq.factory.from(list)); this
			case _                               => super.addAll(elems)
		}
	}
}




/** A `LazyList` reinvented, mostly to have a lazy sequence type under `LazyIterable`.
  * @note Before the reference to this object is exposed outside, a `releaseFence()` must be called by its creator,
  *       and the contents cannot be modified externally after that by any thread, except by `head` and `tail`
  *       evaluating the lazy expressions. This is crucial so that any thread calling one of these methods
  *       will see the most recent values of internal fields.
  */
private sealed class LazySeqLink[+E](private[this] var hd :Term[E], private[this] var tl :Term[LazyLinearSeq[E]])
	extends LazyLinearSeq[E] with Serializable
{
	override protected[this] var initializer :() => LinearSeq[E] = () => toLazyList

	def headTerm :Term[E] = hd
	def headTerm_=(head :Term[E @uncheckedVariance]) :Unit = { hd = head; releaseFence() }
	def tailTerm :Term[LazyLinearSeq[E]] = tl
	def tailTerm_=(tail :Term[LazyLinearSeq[E] @uncheckedVariance]) :Unit = { tl = tail; releaseFence() }

	def acquireHead :Term[E] = { val res = hd; acquireFence(); res }
	def acquireTail :Term[LazyLinearSeq[E]] = { val res = tl; acquireFence(); res }

	//Implementation assumes that either a releaseFence was executed after setting the fields or that this instance
	// was created by the current thread, i.e. we will see properly initialized internals of hd.
	override def head :E = {
		var headTerm = hd
		while (headTerm match {
			case Expression(init) =>
				init.synchronized {
					val currHd = hd
					if (currHd eq headTerm) { //No race condition: swap the Expression for Value under the monitor.
						headTerm = Value(init())
						releaseFence() //We need the fence because threads may synchronize on unrelated instances.
						hd = headTerm
						false
					} else { //Either currHead is already evaluated or the expression changed, and we need to try again.
						headTerm = currHd
						true //If headTerm is a Value, then the next iteration will fall into the following case.
					}
				}
			case _ => false //headTerm is a Value
		}) {}
		headTerm() //headTerm is a Value at this point
	}

	override def tail :LazyLinearSeq[E] = {
		var tailTerm = tl
		while (tailTerm match {
			case Expression(init) =>
				init.synchronized {
					val currTail = tl
					if (currTail eq tailTerm) { //No race condition: swap the Expression for Value under the monitor.
						tailTerm = Value(init())
						releaseFence()
						tl = tailTerm
						false
					} else { //Either currTail is already evaluated to a Value, and we are done, or we need to try again.
						tailTerm = currTail
						true //If tailTerm is a Value, then the next iteration will fall into the following case.
					}
				}
			case _ => false //tailTerm is a Value
		}) {}
		tailTerm() //tailTerm is a Value at this point
	}

	/** A possibly lazy value of the head of this list such that `dependentHead()` and `this.head` will always return
	  * the same object, regardless of the number of times they are called and in what order (i.e., the constructor
	  * will be executed at most once).
	  */
	def dependentHead :Term[E] = {
		var headTerm = hd
		while (headTerm match {
			case Expression(init) =>
				init.synchronized { //Get the monitor so that no one evaluates it while we're not looking.
					val currHead = hd
					if (currHead eq headTerm) { //headTerm is not evaluated, we need a closure delegating to this.head.
						headTerm = Expression(() => head)
						false
					} else { //Either currHead is already evaluated, and we are done, or we need to try again.
						headTerm = currHead
						true //If headTerm is a Value, then the next iteration will fall into the following case.
					}
				}
			case _ => false //headTerm is a Value, we can share it freely.
		}) {}
		headTerm
	}


	/** A possibly lazy tail of this list such that `dependentTail()` and `this.tail` will always return
	  * the same object, regardless of the number of times they are called and in what order (i.e., the constructor
	  * will be executed at most once).
	  */
	def dependentTail :Term[LazyLinearSeq[E]] = {
		var tailTerm = tl
		while (tailTerm match {
			case Expression(init) =>
				init.synchronized {
					val currTail = tl
					if (currTail eq tailTerm) {
						tailTerm = Expression(() => tail)
						false
					} else {
						tailTerm = currTail
						true
					}
				}
			case _ => false
		}) {}
		tailTerm
	}

	override def isEmpty = false
 	override def knownSize :Int = acquireTail match {
		case Expression(_)               => -1
		case Value(tail) if tail.isEmpty => 0
		case _                           => -1
	}

	override def length :Int = {
		@tailrec def countElems(seq :LazyLinearSeq[E], res :Int) :Int = seq match {
			case link :LazySeqLink[E] => countElems(link.tail, res + 1)
			case _ if seq.isEmpty     => 0
			case _                    => countElems(seq.tail, res + 1)
		}
		knownSize match {
			case -1  => countElems(tail, 1)
			case len => len
		}
	}

	override def apply(i :Int) :E =
		if (i < 0)
			outOfBounds_!(i, this, "apply")
		else if (i == 0)
			head
		else
			tail(i - 1)

	//todo: review methods in LazyListOps and see which warrant copying.

	override def toLazyList :LazyList[E] = head #:: tail.toLazyList

}
