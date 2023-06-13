package net.noresttherein.sugar.testing.scalacheck

import scala.collection.immutable.AbstractSet
import scala.reflect.{ClassTag, classTag}
import scala.Console.err

import net.noresttherein.sugar.extensions.ClassExtension
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf
import org.scalacheck.{Gen, Prop}
import org.scalacheck.util.Pretty


object extensions {
	type Prettify[X] = X => Pretty

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

		def orElse(p : => Prop) =  //Prop.secure(self) || Prop.secure(p)
			self.combine(Prop.secure(p)) { (first, second) =>
			if (first.failure && second.status.isInstanceOf[Prop.Exception])
				err.println(first.toString + " orElse " + second)
			if (first.success) first
			else second
		}
	}

	implicit class BooleanAsPropExtension(private val self :Boolean) extends AnyVal {
		@inline def lbl(l : => String) :Prop = Prop(self) lbl l
		@inline def lbl_:(l : => String) :Prop = lbl(l)
		@inline def :@(l : => String) :Prop = lbl(l)
		@inline def @:(l : => String) :Prop = lbl(l)

		@inline def orElse(p : => Prop) :Prop =  Prop(self) orElse p
	}

	implicit class LazyExtension(self : => Any) {
		def throws[E <: Throwable :ClassTag] :Prop = {
			lazy val expr = self
			val thrown = Prop.throws(classTag[E].runtimeClass.asInstanceOf[Class[E]])(expr)
			if (thrown) Prop.passed
			else Prop.falsified lbl "throws " + fullNameOf[E] lbl "returned: " + expr
		}
	}


	class GenDisjunction[+X](val alternatives :Seq[Gen[X]]) {
		def size :Int = alternatives.length
		def ||[U >: X](gen :Gen[U]) :GenDisjunction[U] = new GenDisjunction(alternatives :+ gen)
	}

	implicit def GenToGenDisjunction[X](gen :Gen[X]) :GenDisjunction[X] =
		new GenDisjunction(Vector(gen))

	implicit def GenDisjunctionToGen[X](gens :GenDisjunction[X]) :Gen[X] =
		Gen.choose(0, gens.size - 1).flatMap(gens.alternatives)
}
