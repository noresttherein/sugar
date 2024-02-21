package net.noresttherein.sugar.collections

import scala.annotation.tailrec
import scala.collection.{AbstractView, SeqView, View}

import net.noresttherein.sugar.outOfBounds_!
import net.noresttherein.sugar.vars.Opt





/** Views implementing effects of various extension methods from this package. */
private object Views {

	/** A convenience base class allowing to define views as zero-argument function literals.
	  * This class is ''not'' a part of public API.
	  */
	abstract class BaseView[+E] extends AbstractView[E] {
		override def iterator :Iterator[E] = newIterator()
		protected def newIterator() :Iterator[E] //SAM conversion kicks in only for methods with a parameter group.
	}
	abstract class BaseSeqView[+E] extends BaseView[E] with SeqView[E] {
		override def apply(i :Int) :E =
			try newIterator().drop(i).next() catch {
				case e :NoSuchElementException => outOfBounds_!(i.toString, e)
			}
		override def length :Int = newIterator().size
	}

	def single[A](elem :A) :BaseView[A] = () => Iterator.single(elem)
	def double[A](first :A, second :A) :BaseView[A] = () => Iterators.double(first, second)

	@tailrec def zipEven[A, B](left :IterableOnce[A], right :IterableOnce[B]) :BaseView[(A, B)] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipEven(left.iterator, right.iterator)
				case _              => zipEven(left, TemporaryBuffer from right)
			}
			case _ => zipEven(TemporaryBuffer from left, right)
		}

	@tailrec def zipMap[A, B, O](left :IterableOnce[A], right :IterableOnce[B])(f :(A, B) => O) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipMap(left.iterator, right.iterator)(f)
				case _              => zipMap(left, TemporaryBuffer from right)(f)
			}
			case _ => zipMap(TemporaryBuffer from left, right)(f)
		}

	@tailrec def zipMapEven[A, B, O](left :IterableOnce[A], right :IterableOnce[B])(f :(A, B) => O) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipMapEven(left.iterator, right.iterator)(f)
				case _              => zipMapEven(left, TemporaryBuffer from right)(f)
			}
			case _ => zipMapEven(TemporaryBuffer from left, right)(f)
		}

	@tailrec def zipMapAll[A, B, O](left :IterableOnce[A], right :IterableOnce[B], leftElem :A, rightElem :B)
	                               (f :(A, B) => O) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipMapAll(left.iterator, right.iterator, leftElem, rightElem)(f)
				case _              => zipMapAll(left, TemporaryBuffer from right, leftElem, rightElem)(f)
			}
			case _ => zipMapAll(TemporaryBuffer from left, right, leftElem, rightElem)(f)
		}

	@tailrec def zipFlatMap[A, B, O](left :IterableOnce[A], right :IterableOnce[B])
	                                (f :(A, B) => IterableOnce[O]) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipFlatMap(left.iterator, right.iterator)(f)
				case _              => zipFlatMap(left, TemporaryBuffer from right)(f)
			}
			case _ => zipFlatMap(TemporaryBuffer from left, right)(f)
		}

	@tailrec def zipFlatMapEven[A, B, O](left :IterableOnce[A], right :IterableOnce[B])
	                                    (f :(A, B) => IterableOnce[O]) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] => () => Iterators.zipFlatMapEven(left.iterator, right.iterator)(f)
				case _              => zipFlatMapEven(left, TemporaryBuffer from right)(f)
			}
			case _ => zipFlatMapEven(TemporaryBuffer from left, right)(f)
		}

	@tailrec def zipFlatMapAll[A, B, O](left :IterableOnce[A], right :IterableOnce[B], leftElem :A, rightElem :B)
	                                   (f :(A, B) => IterableOnce[O]) :BaseView[O] =
		left match {
			case _ :Iterable[A] => right match {
				case _ :Iterable[B] =>
					() => Iterators.zipFlatMapAll(left.iterator, right.iterator, leftElem, rightElem)(f)
				case _              =>
					zipFlatMapAll(left, TemporaryBuffer from right, leftElem, rightElem)(f)
			}
			case _ => zipFlatMapAll(TemporaryBuffer from left, right, leftElem, rightElem)(f)
		}

	@tailrec def zip3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C])
			:BaseView[(A, B, C)] =
		first match {
			case _ :Iterable[A] => second match {
				case _ :Iterable[B] => third match {
					case _ :Iterable[C] => () => Iterators.zip3(first.iterator, second.iterator, third.iterator)
					case _              => zip3(first, second, TemporaryBuffer from third)
				}
				case _ => zip3(first, TemporaryBuffer from second, third)
			}

			case _ => zip3(TemporaryBuffer from first, second, third)
		}

	@tailrec def zipEven3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C])
			:BaseView[(A, B, C)] =
		first match {
			case _ :Iterable[A] => second match {
				case _ :Iterable[B] => third match {
					case _ :Iterable[C] => () => Iterators.zipEven3(first.iterator, second.iterator, third.iterator)
					case _              => zipEven3(first, second, TemporaryBuffer from third)
				}
				case _ => zipEven3(first, TemporaryBuffer from second, third)
			}

			case _ => zipEven3(TemporaryBuffer from first, second, third)
		}

	@tailrec def zipAll3[A, B, C](first :IterableOnce[A], second :IterableOnce[B], third :IterableOnce[C],
	                              firstElem :A, secondElem :B, thirdElem :C) :BaseView[(A, B, C)] =
		first match {
			case _ :Iterable[A] => second match {
				case _ :Iterable[B] => third match {
					case _ :Iterable[C] =>
						() => Iterators.zipAll3(
							first.iterator, second.iterator, third.iterator, firstElem, secondElem, thirdElem
						)
					case _              =>
						zipAll3(first, second, TemporaryBuffer from third, firstElem, secondElem, thirdElem)
				}
				case _ => zipAll3(first, TemporaryBuffer from second, third, firstElem, secondElem, thirdElem)
			}

			case _ => zipAll3(TemporaryBuffer from first, second, third, firstElem, secondElem, thirdElem)
		}

	@tailrec def zipTail[A](self :IterableOnce[A]) :BaseView[(A, A)] = self match {
		case _ :Iterable[A] => () => Iterators.zipTail(self.iterator)
		case _              => zipTail(TemporaryBuffer from self)
	}


	@tailrec def mapWith[A, B, C](self :IterableOnce[A], z :B, f :(A, B) => (C, B)) :BaseView[C] = self match {
		case _ :Iterable[A] => () => Iterators.mapWith(self.iterator, z, f)
		case _              => mapWith(TemporaryBuffer from self, z, f)
	}

	@tailrec def flatMapWith[A, B, C](self :IterableOnce[A], z :B, f :(A, B) => (IterableOnce[C], B)) :BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapWith(self.iterator, z, f)
			case _              => flatMapWith(TemporaryBuffer from self, z, f)
		}

	@tailrec def mapWithIndex[A, B](self :IterableOnce[A], f :(A, Int) => B) :BaseView[B] =
		self match {
			case _ :Iterable[A] => () => Iterators.mapWithIndex(self.iterator, f)
			case _              => mapWithIndex(TemporaryBuffer from self, f)
		}

	@tailrec def flatMapWithIndex[A, B](self :IterableOnce[A], f :(A, Int) => IterableOnce[B]) :BaseView[B] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapWithIndex(self.iterator, f)
			case _              => flatMapWithIndex(TemporaryBuffer from self, f)
		}

	@tailrec def collectWithIndex[A, B](self :IterableOnce[A], f :PartialFunction[(A, Int), B]) :BaseView[B] =
		self match {
			case _ :Iterable[A] => () => Iterators.collectWithIndex(self.iterator, f)
			case _              => collectWithIndex(TemporaryBuffer from self, f)
		}

	@tailrec def mapWhile[A, B, C](self :IterableOnce[A], z :B, pred :B => Boolean, f :(B, A) => (B, C)) :BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.mapWhile(self.iterator, z, pred, f)
			case _              => mapWhile(TemporaryBuffer from self, z, pred, f)
		}

	@tailrec def flatMapWhile[A, B, C](self :IterableOnce[A], z :B, pred :B => Boolean, f :(B, A) => (B, IterableOnce[C]))
			:BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapWhile(self.iterator, z, pred, f)
			case _              => flatMapWhile(TemporaryBuffer from self, z, pred, f)
		}

	@tailrec def mapUntil[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => (Boolean, B, C)) :BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.mapUntil(self.iterator, z, f)
			case _              => mapUntil(TemporaryBuffer from self, z, f)
		}

	@tailrec def flatMapUntil[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => (Boolean, B, IterableOnce[C]))
			:BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapUntil(self.iterator, z, f)
			case _              => flatMapUntil(TemporaryBuffer from self, z, f)
		}

	@tailrec def mapSome[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Option[(B, C)]) :BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.mapSome(self.iterator, z, f)
			case _              => mapSome(TemporaryBuffer from self, z, f)
		}

	@tailrec def flatMapSome[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Option[(B, IterableOnce[C])])
			:BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapSome(self.iterator, z, f)
			case _              => flatMapSome(TemporaryBuffer from self, z, f)
		}

	@tailrec def mapPrefix[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Opt[(B, C)]) :BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.mapPrefix(self.iterator, z, f)
			case _              => mapPrefix(TemporaryBuffer from self, z, f)
		}

	@tailrec def flatMapPrefix[A, B, C](self :IterableOnce[A], z :B, f :(B, A) => Opt[(B, IterableOnce[C])])
			:BaseView[C] =
		self match {
			case _ :Iterable[A] => () => Iterators.flatMapPrefix(self.iterator, z, f)
			case _              => flatMapPrefix(TemporaryBuffer from self, z, f)
		}


	@tailrec def filterWith[A, B](self :IterableOnce[A], z :B, pred :(A, B) => (Boolean, B), keep :Boolean) :BaseView[A] =
		self match {
			case _ :Iterable[A] => () => Iterators.filterWith(self.iterator, z, pred, keep)
			case _              => filterWith(TemporaryBuffer from self, z, pred, keep)
		}

	@tailrec def filterWithIndex[A](self :IterableOnce[A], pred :(A, Int) => Boolean, keep :Boolean) :BaseView[A] =
		self match {
			case _ :Iterable[A] => () => Iterators.filterWithIndex(self.iterator, pred, keep)
			case _              => filterWithIndex(TemporaryBuffer from self, pred, keep)
		}

	@tailrec def keep[A](self :IterableOnce[A], pred :Int => Boolean) :View[A] =
		self match {
			case _ if self.knownSize == 0 => View.empty
			case _ :Iterable[A] => (() => Iterators.keep(self.iterator, pred)) :BaseView[A]
			case _              => keep(TemporaryBuffer from self, pred)
		}

	@tailrec def distinct[A](self :IterableOnce[A]) :View[A] = {
		val size = self.knownSize
		if (size >= 0 && size <= 1) View from self
		else if (self.isInstanceOf[Iterable[_]]) (() => Iterators.distinct(self.iterator)) :BaseView[A]
		else distinct(TemporaryBuffer from self)
	}


	@tailrec def removed[A](self :IterableOnce[A], index :Int) :BaseView[A] = self match {
		case _ :Iterable[A] => () => Iterators.removed(self.iterator, index)
		case _              => removed(TemporaryBuffer from self, index)
	}

	@tailrec def removed[A](self :IterableOnce[A], from :Int, until :Int) :BaseView[A] = self match {
		case _ :Iterable[A] => () => Iterators.removed(self.iterator, from, until)
		case _              => removed(TemporaryBuffer from self, from, until)
	}


	@tailrec def updated[A](self :IterableOnce[A], index :Int, elem :A) :BaseSeqView[A] = self match {
		case _ :Iterable[A] => () => Iterators.updated(self.iterator, index, elem)
		case _              => updated(TemporaryBuffer from self, index, elem)
	}

	@tailrec def updatedAll[A](self :IterableOnce[A], index :Int, elems :IterableOnce[A]) :BaseSeqView[A] = self match {
		case _ :Iterable[A] => elems match {
			case _ :Iterable[A] => () => Iterators.updatedAll(self.iterator, index, elems)
			case _              => updatedAll(self, index, TemporaryBuffer from elems)
		}
		case _ => updatedAll(TemporaryBuffer from self, index, elems)
	}

	@tailrec def overwritten[A](self :IterableOnce[A], index :Int, elems :IterableOnce[A]) :BaseSeqView[A] =
		self match {
			case _ :Iterable[A] => elems match {
				case _ :Iterable[A] => () => Iterators.overwritten(self.iterator, index, elems)
				case _              => overwritten(self, index, TemporaryBuffer from elems)
			}
			case _ => overwritten(TemporaryBuffer from self, index, elems)
		}

	@tailrec def inserted[E](self :IterableOnce[E], index :Int, elem :E) :BaseSeqView[E] = self match {
		case _ :Iterable[_] => () => Iterators.inserted(self.iterator, index, elem)
		case _              => inserted(TemporaryBuffer from self, index, elem)
	}

	@tailrec def insertedAll[E](self :IterableOnce[E], index :Int, elems :IterableOnce[E]) :BaseSeqView[E] =
		self match {
			case _ :Iterable[_] => elems match {
				case _ :Iterable[_] => () => Iterators.insertedAll(self.iterator, index, elems)
				case _ => insertedAll(self, index, TemporaryBuffer from elems)
			}
			case _ => insertedAll(TemporaryBuffer from self, index, elems)
		}

}
