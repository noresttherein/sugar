package net.noresttherein.sugar.testing.scalacheck

import scala.reflect.{ClassTag, classTag}
import scala.Console.err

import net.noresttherein.sugar.collections.LazySet
import net.noresttherein.sugar.extensions.immutableSetExtension
import net.noresttherein.sugar.reflect.prettyprint.fullNameOf
import org.scalacheck.{Gen, Prop}
import org.scalacheck.util.Pretty


object extensions {
	//todo: support for color terminal pull request to scalacheck.scala
	type Prettify[X] = X => Pretty

	implicit class PropExtension(private val self :Prop) extends AnyVal {
		def lbl(l : => String) :Prop = self.map { res =>
			res.copy(labels = res.labels +~ l)
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
			lazy val format =
				try expr.toString catch {
					case e :Exception => "<toString threw " + e + ">"
				}
			val thrown = Prop.throws(classTag[E].runtimeClass.asInstanceOf[Class[E]])(expr)
			if (thrown) Prop.passed
			else Prop.falsified lbl "throws " + fullNameOf[E] lbl "returned: " + format
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
