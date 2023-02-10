package net.noresttherein.sugar.testing.scalacheck

import scala.collection.immutable.AbstractSet
import scala.reflect.{classTag, ClassTag}

import org.scalacheck.Prop


object extensions {
	private class LazySet[T](lzy : => Set[T]) extends AbstractSet[T] {
		private lazy val evaluated = lzy
		override def contains(elem :T) :Boolean = evaluated.contains(elem)
		override def incl(elem :T) :Set[T] = new LazySet(evaluated + elem)
		override def excl(elem :T) :Set[T] = new LazySet(evaluated - elem)
//		def add(elem : => T) :Set[T] = new LazySet(evaluated + elem)
		override def iterator :Iterator[T] = evaluated.iterator
		override def empty = new LazySet[T](Set.empty[T])
	}

	implicit class PropExtension(private val self :Prop) extends AnyVal {
		def lbl(l : => String) :Prop = self.map { res =>
			res.copy(labels = new LazySet(res.labels + l))
		}
		@inline def lbl_:(l: => String) :Prop = lbl(l)
		@inline def :@(l: => String) :Prop = lbl(l)
		@inline def @:(l: => String) :Prop = lbl(l)
	}

	implicit class LazyExtension(self : => Any) {
		def throws[E <: Throwable :ClassTag] :Prop =
			Prop(Prop.throws(classTag[E].runtimeClass.asInstanceOf[Class[E]])(self))
	}

}
