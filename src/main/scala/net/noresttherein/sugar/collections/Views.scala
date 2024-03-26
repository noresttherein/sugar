package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.{AbstractView, IterableOnceOps, SeqView, View}

import net.noresttherein.sugar.extensions.{IterableOnceExtension, IteratorExtension}
import net.noresttherein.sugar.funny.generic.Any1
import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.vars.Opt




/** A convenience base class allowing to define views as zero-argument function literals.
  * This class is ''not'' a part of public API.
  */
private[collections] abstract class InlineView[+E] extends AbstractView[E] {
	override def iterator :Iterator[E] = newIterator()
	protected def newIterator() :Iterator[E] //SAM conversion kicks in only for methods with a parameter group.
}
private[collections] abstract class InlineSeqView[+E] extends InlineView[E] with SeqView[E] {
	override def apply(i :Int) :E =
		try newIterator().drop(i).next() catch {
			case e :NoSuchElementException => outOfBounds_!(i.toString, e)
		}
	override def length :Int = newIterator().size
}


/** Views implementing effects of various extension methods from this package. */
private object Views {

	def single[A](elem :A) :InlineView[A] = () => Iterator.single(elem)
	def double[A](first :A, second :A) :InlineView[A] = () => Iterators.double(first, second)

	@tailrec def zipEven[A, B](left :IterableOnce[A], right :IterableOnce[B]) :InlineView[(A, B)] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipEven(left.iterator, right.iterator)
				case _ => zipEven(left, LazyList from right)
			}
			case _ => zipEven(LazyList from left, right)
		}

	@tailrec def zipMap[A, B, O](left :IterableOnce[A], right :IterableOnce[B])(f :(A, B) => O) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipMap(left.iterator, right.iterator)(f)
				case _ => zipMap(left, LazyList from right)(f)
			}
			case _ => zipMap(LazyList from left, right)(f)
		}

	@tailrec def zipMapEven[A, B, O](left :IterableOnce[A], right :IterableOnce[B])(f :(A, B) => O) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipMapEven(left.iterator, right.iterator)(f)
				case _ => zipMapEven(left, LazyList from right)(f)
			}
			case _ => zipMapEven(LazyList from left, right)(f)
		}

	@tailrec def zipMapAll[A, B, O](left :IterableOnce[A], right :IterableOnce[B], leftElem :A, rightElem :B)
	                               (f :(A, B) => O) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipMapAll(left.iterator, right.iterator, leftElem, rightElem)(f)
				case _ => zipMapAll(left, LazyList from right, leftElem, rightElem)(f)
			}
			case _ => zipMapAll(LazyList from left, right, leftElem, rightElem)(f)
		}

	@tailrec def zipFlatMap[A, B, O](left :IterableOnce[A], right :IterableOnce[B])
	                                (f :(A, B) => IterableOnce[O]) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipFlatMap(left.iterator, right.iterator)(f)
				case _ => zipFlatMap(left, LazyList from right)(f)
			}
			case _ => zipFlatMap(LazyList from left, right)(f)
		}

	@tailrec def zipFlatMapEven[A, B, O](left :IterableOnce[A], right :IterableOnce[B])
	                                    (f :(A, B) => IterableOnce[O]) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipFlatMapEven(left.iterator, right.iterator)(f)
				case _ => zipFlatMapEven(left, LazyList from right)(f)
			}
			case _ => zipFlatMapEven(LazyList from left, right)(f)
		}

	@tailrec def zipFlatMapAll[A, B, O](left :IterableOnce[A], right :IterableOnce[B], leftElem :A, rightElem :B)
	                                   (f :(A, B) => IterableOnce[O]) :InlineView[O] =
		left match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => right match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.zipFlatMapAll(left.iterator, right.iterator, leftElem, rightElem)(f)
				case _ => zipFlatMapAll(left, LazyList from right, leftElem, rightElem)(f)
			}
			case _ => zipFlatMapAll(LazyList from left, right, leftElem, rightElem)(f)
		}

	@tailrec def zip3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C])
			:InlineView[(A, B, C)] =
		first match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => second match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => third match {
					case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
						() => Iterators.zip3(first.iterator, second.iterator, third.iterator)
					case _ => zip3(first, second, LazyList from third)
				}
				case _ => zip3(first, LazyList from second, third)
			}
			case _ => zip3(LazyList from first, second, third)
		}

	@tailrec def zipEven3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C])
			:InlineView[(A, B, C)] =
		first match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => second match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => third match {
					case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
						() => Iterators.zipEven3(first.iterator, second.iterator, third.iterator)
					case _ => zipEven3(first, second, LazyList from third)
				}
				case _ => zipEven3(first, LazyList from second, third)
			}
			case _ => zipEven3(LazyList from first, second, third)
		}

	@tailrec def zipAll3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C],
	                              firstElem :A, secondElem :B, thirdElem :C) :InlineView[(A, B, C)] =
		first match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => second match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => third match {
					case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
						() => Iterators.zipAll3(
							first.iterator, second.iterator, third.iterator, firstElem, secondElem, thirdElem
						)
					case _ => zipAll3(first, second, LazyList from third, firstElem, secondElem, thirdElem)
				}
				case _ => zipAll3(first, LazyList from second, third, firstElem, secondElem, thirdElem)
			}
			case _ => zipAll3(LazyList from first, second, third, firstElem, secondElem, thirdElem)
		}

	@tailrec def zipTail[A](self :IterableOnce[A]) :InlineView[(A, A)] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.zipTail(self.iterator)
		case _ => zipTail(LazyList from self)
	}


	@tailrec def mapWith[A, B, C](self :IterableOnce[A], z :B, f :(A, B) => (C, B)) :InlineView[C] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.mapWith(self.iterator, z, f)
		case _ => mapWith(LazyList from self, z, f)
	}

	@tailrec def flatMapWith[A, B, C](self :IterableOnce[A], z :B, f :(A, B) => (IterableOnce[C], B)) :InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapWith(self.iterator, z, f)
			case _ => flatMapWith(LazyList from self, z, f)
		}

	@tailrec def mapWithIndex[A, B](self :IterableOnce[A], f :(A, Int) => B) :InlineView[B] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.mapWithIndex(self.iterator, f)
			case _ => mapWithIndex(LazyList from self, f)
		}

	@tailrec def flatMapWithIndex[A, B](self :IterableOnce[A], f :(A, Int) => IterableOnce[B]) :InlineView[B] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapWithIndex(self.iterator, f)
			case _ => flatMapWithIndex(LazyList from self, f)
		}

	@tailrec def collectWithIndex[A, B](self :IterableOnce[A], f :PartialFunction[(A, Int), B]) :InlineView[B] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.collectWithIndex(self.iterator, f)
			case _ => collectWithIndex(LazyList from self, f)
		}

	@tailrec def mapWhile[A, B, C](self :IterableOnce[A], z :B, pred :B => Boolean, f :(B, A) => (B, C)) :InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.mapWhile(self.iterator, z, pred, f)
			case _ => mapWhile(LazyList from self, z, pred, f)
		}

	@tailrec def flatMapWhile[A, B, C](self :IterableOnce[A], z :B, pred :B => Boolean, f :(B, A) => (B, IterableOnce[C]))
			:InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapWhile(self.iterator, z, pred, f)
			case _ => flatMapWhile(LazyList from self, z, pred, f)
		}

	@tailrec def mapUntil[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => (Boolean, B, C)) :InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.mapUntil(self.iterator, z, f)
			case _ => mapUntil(LazyList from self, z, f)
		}

	@tailrec def flatMapUntil[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => (Boolean, B, IterableOnce[C]))
			:InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapUntil(self.iterator, z, f)
			case _ => flatMapUntil(LazyList from self, z, f)
		}

	@tailrec def mapSome[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Option[(B, C)]) :InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.mapSome(self.iterator, z, f)
			case _ => mapSome(LazyList from self, z, f)
		}

	@tailrec def flatMapSome[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Option[(B, IterableOnce[C])])
			:InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapSome(self.iterator, z, f)
			case _ => flatMapSome(LazyList from self, z, f)
		}

	@tailrec def mapPrefix[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Opt[(B, C)]) :InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.mapPrefix(self.iterator, z, f)
			case _ => mapPrefix(LazyList from self, z, f)
		}

	@tailrec def flatMapPrefix[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Opt[(B, IterableOnce[C])])
			:InlineView[C] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.flatMapPrefix(self.iterator, z, f)
			case _ => flatMapPrefix(LazyList from self, z, f)
		}


	@tailrec def filterWith[A, B](self :IterableOnce[A], z :B, pred :(A, B) => (Boolean, B), keep :Boolean) :InlineView[A] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.filterWith(self.iterator, z, pred, keep)
			case _ => filterWith(LazyList from self, z, pred, keep)
		}

	@tailrec def filterWithIndex[A](self :IterableOnce[A], pred :(A, Int) => Boolean, keep :Boolean) :InlineView[A] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.filterWithIndex(self.iterator, pred, keep)
			case _ => filterWithIndex(LazyList from self, pred, keep)
		}

	@tailrec def keep[A](self :IterableOnce[A], pred :Int => Boolean) :View[A] =
		self match {
			case _ if self.knownSize == 0 => View.empty
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				(() => Iterators.keep(self.iterator, pred)) :InlineView[A]
			case _ => keep(LazyList from self, pred)
		}

	@tailrec def distinct[A](self :IterableOnce[A]) :View[A] = self match {
		case _ if { val size = self.knownSize; size >= 0 & size <= 1 } => View from self
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			(() => Iterators.distinct(self.iterator)) :InlineView[A]
		case _ => distinct(LazyList from self)
	}


	@tailrec def removed[A](self :IterableOnce[A], index :Int) :InlineView[A] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.removed(self.iterator, index)
		case _ => removed(LazyList from self, index)
	}

	@tailrec def removed[A](self :IterableOnce[A], from :Int, until :Int) :InlineView[A] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.removed(self.iterator, from, until)
		case _ => removed(LazyList from self, from, until)
	}


	@tailrec def updated[A](self :IterableOnce[A], index :Int, elem :A) :InlineSeqView[A] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.updated(self.iterator, index, elem)
		case _ => updated(LazyList from self, index, elem)
	}

	@tailrec def updatedAll[A](self :IterableOnce[A], index :Int, elems :IterableOnce[A]) :InlineSeqView[A] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => elems match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.updatedAll(self.iterator, index, elems)
			case _ => updatedAll(self, index, LazyList from elems)
		}
		case _ => updatedAll(LazyList from self, index, elems)
	}

	@tailrec def overwritten[A](self :IterableOnce[A], index :Int, elems :IterableOnce[A]) :InlineSeqView[A] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => elems match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.overwritten(self.iterator, index, elems)
				case _ => overwritten(self, index, LazyList from elems)
			}
			case _ => overwritten(LazyList from self, index, elems)
		}

	@tailrec def inserted[E](self :IterableOnce[E], index :Int, elem :E) :InlineSeqView[E] = self match {
		case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
			() => Iterators.inserted(self.iterator, index, elem)
		case _ => inserted(LazyList from self, index, elem)
	}

	@tailrec def insertedAll[E](self :IterableOnce[E], index :Int, elems :IterableOnce[E]) :InlineSeqView[E] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain => elems match {
				case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
					() => Iterators.insertedAll(self.iterator, index, elems)
				case _ => insertedAll(self, index, LazyList from elems)
			}
			case _ => insertedAll(LazyList from self, index, elems)
		}

	@tailrec def takeWith[A, E](self :IterableOnce[E], z :A, pred :A => Boolean, op :(A, E) => A) :InlineView[E] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.takeWith(self.iterator, z, pred, op)
			case _ => takeWith(LazyList from self, z, pred, op)
		}
	@tailrec def takeUntil[A, E](self :IterableOnce[E], z :A, pred :A => Boolean, op :(A, E) => A) :InlineView[E] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => Iterators.takeUntil(self.iterator, z, pred, op)
			case _ => takeUntil(LazyList from self, z, pred, op)
		}

	@tailrec def dropWith[A, E](self :IterableOnce[E], z :A, pred :A => Boolean, op :(A, E) => A) :InlineView[E] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => items.iterator.dropWith(z)(pred)(op)
			case _ => dropWith(LazyList from self, z, pred, op)
		}
	@tailrec def dropUntil[A, E](self :IterableOnce[E], z :A, pred :A => Boolean, op :(A, E) => A) :InlineView[E] =
		self match {
			case items :IterableOnceOps[_, Any1, _] if items.isTraversableAgain =>
				() => items.iterator.dropUntil(z)(pred)(op)
			case _ => dropUntil(LazyList from self, z, pred, op)
		}

}
