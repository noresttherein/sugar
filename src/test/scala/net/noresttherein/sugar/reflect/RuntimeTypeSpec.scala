package net.noresttherein.sugar.reflect

import java.time.Instant
import java.util.Date

import scala.reflect._
import scala.reflect.runtime.universe.TypeTag
import scala.runtime.BoxedUnit

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._




object RuntimeTypeSpec extends Properties("RuntimeType") {



	implicit class ClassComparator(private val self :Class[_]) extends AnyVal {
		def is[T](other :Class[T]) :Prop = self.asInstanceOf[Class[T]] ?= other
	}

	implicit class RuntimeTypeProps[E](private val self :RuntimeType[E]) extends AnyVal {
		def is[T](other :Class[T]) :Prop =
			(self.runType is other) :| s"$self.toType is $other" &&
				(self.emptyArray.getClass.getComponentType is other) :| s"$self.array element is $other" &&
				(self.newArray(2).getClass.getComponentType is other) :| s"$self.newArray element is $other" &&
				(self.classTag.asInstanceOf[ClassTag[T]] ?= ClassTag(other)) :| s"$self.classTag is ClassTag($other)"
	}

	def unitProp(self :RuntimeType[Unit]) :Prop = {
		val other = classOf[Unit]
		val box = classOf[BoxedUnit]
		Prop(self.isUnit) :| s"$self.isUnit is true" &&
			(self.runType is other) :| s"$self.toType is $other)" &&
			(self.emptyArray.getClass.getComponentType is box) :| s"$self.array element is $box" &&
			(self.newArray(2).getClass.getComponentType is box) :| s"$self.newArray element is $box" &&
			(self.classTag.asInstanceOf[ClassTag[Unit]] ?= ClassTag(other)) :| s"$self.classTag is ClassTag($other)"
	}


	//sanity checks to verify scala performs AnyVal - java primitive conversion as we expect

	property("scala.Byte") = (Primitives.Byte is classOf[Byte]) && (Primitives.Byte is classTag[Byte].runtimeClass)
	property("scala.Short") = (Primitives.Short is classOf[Short]) && (Primitives.Short is classTag[Short].runtimeClass)
	property("scala.Int") = (Primitives.Int is classOf[Int]) && (Primitives.Int is classTag[Int].runtimeClass)
	property("scala.Long") = (Primitives.Long is classOf[Long]) && (Primitives.Long is classTag[Long].runtimeClass)
	property("scala.Char") = (Primitives.Char is classOf[Char]) && (Primitives.Char is classTag[Char].runtimeClass)
	property("scala.Float") = (Primitives.Float is classOf[Float]) && (Primitives.Float is classTag[Float].runtimeClass)
	property("scala.Double") = (Primitives.Double is classOf[Double]) && (Primitives.Double is classTag[Double].runtimeClass)
	property("scala.Boolean") = (Primitives.Boolean is classOf[Boolean]) && (Primitives.Boolean is classTag[Boolean].runtimeClass)
	property("scala.Unit") = (Primitives.Unit is classOf[Unit]) && (Primitives.Unit is classTag[Unit].runtimeClass)
	property("scala.Any") = (Primitives.Any is classOf[AnyRef]) && (Primitives.Any is classTag[Any].runtimeClass)
	property("scala.AnyRef") = (Primitives.Any is classOf[AnyRef]) && (Primitives.Any is classTag[AnyRef].runtimeClass)

	/** All possible values, different from each other. */
	property("Specializations") = RuntimeType.Specializations.size == 10


	/** Implicit accessor with explicit types. */

	property("apply[Byte]") = RuntimeType[Byte] is classOf[Byte]
	property("apply[Short]") = RuntimeType[Short] is classOf[Short]
	property("apply[Int]") = RuntimeType[Int] is classOf[Int]
	property("apply[Long]") = RuntimeType[Long] is classOf[Long]
	property("apply[Char]") = RuntimeType[Char] is classOf[Char]
	property("apply[Float]") = RuntimeType[Float] is classOf[Float]
	property("apply[Double]") = RuntimeType[Double] is classOf[Double]
	property("apply[Boolean]") = RuntimeType[Boolean] is classOf[Boolean]
	property("apply[Unit]") = unitProp(RuntimeType[Unit])
	property("apply[Any]") = RuntimeType[Any] is classOf[AnyRef]
	property("apply[AnyRef]") = RuntimeType[AnyRef] is classOf[AnyRef]
	property("apply[String]") = RuntimeType[String] is classOf[String]



	def specialized[@specialized T] :RuntimeType[T] = RuntimeType[T]

	property("apply[@specialized T]") = {
		import RuntimeTypeSpec.{specialized => spec}
		(spec[Byte] is classOf[Byte]) &&
			(spec[Short] is classOf[Short]) &&
			(spec[Int] is classOf[Int]) &&
			(spec[Long] is classOf[Long]) &&
			(spec[Char] is classOf[Char]) &&
			(spec[Float] is classOf[Float]) &&
			(spec[Double] is classOf[Double]) &&
			(spec[Boolean] is classOf[Boolean]) &&
			unitProp(spec[Unit]) &&
			(spec[Any] is classOf[AnyRef]) &&
			(spec[AnyRef] is classOf[AnyRef]) &&
			(spec[String] is classOf[AnyRef])
	}


	property("apply[T :TypeTag]") = {
		def spec[T :TypeTag] :RuntimeType[T] = RuntimeType[T]

		(spec[Byte] is classOf[Byte]) &&
			(spec[Short] is classOf[Short]) &&
			(spec[Int] is classOf[Int]) &&
			(spec[Long] is classOf[Long]) &&
			(spec[Char] is classOf[Char]) &&
			(spec[Float] is classOf[Float]) &&
			(spec[Double] is classOf[Double]) &&
			(spec[Boolean] is classOf[Boolean]) &&
			unitProp(spec[Unit]) &&
			(spec[Any] is classOf[AnyRef]) &&
			(spec[AnyRef] is classOf[AnyRef]) &&
			(spec[String] is classOf[String])
	}



	property("apply[T :ClassTag]") = {
		def spec[T :ClassTag] :RuntimeType[T] = RuntimeType[T]

		(spec[Byte] is classOf[Byte]) &&
			(spec[Short] is classOf[Short]) &&
			(spec[Int] is classOf[Int]) &&
			(spec[Long] is classOf[Long]) &&
			(spec[Char] is classOf[Char]) &&
			(spec[Float] is classOf[Float]) &&
			(spec[Double] is classOf[Double]) &&
			(spec[Boolean] is classOf[Boolean]) &&
			unitProp(spec[Unit]) &&
			(spec[Any] is classOf[AnyRef]) &&
			(spec[AnyRef] is classOf[AnyRef]) &&
			(spec[String] is classOf[String])
	}


	property("apply[T :Specialization]") = {
		def spec[T :RuntimeType] = RuntimeType[T]

		(spec(RuntimeType.OfByte) is classOf[Byte]) &&
			(spec(RuntimeType.OfAny) is classOf[AnyRef])
	}


	def specializedWithTypeClass[@specialized T :RuntimeType] = RuntimeType[T]

	property("apply[@specialized T :RuntimeType]") = {
		import RuntimeTypeSpec.{specializedWithTypeClass => spec}
		(spec[Byte] is classOf[Byte]) &&
			(spec[Any] is classOf[AnyRef]) &&
			(spec[Byte](RuntimeType.erased) is classOf[AnyRef]) :| "type class should take precedence over specialization"
	}


	/** Local specialization context */

	import RuntimeType.specialized
	property("specialized[Byte]") = specialized[Byte] is classOf[Byte]
	property("specialized[Short]") = specialized[Short] is classOf[Short]
	property("specialized[Int]") = specialized[Int] is classOf[Int]
	property("specialized[Long]") = specialized[Long] is classOf[Long]
	property("specialized[Char]") = specialized[Char] is classOf[Char]
	property("specialized[Float]") = specialized[Float] is classOf[Float]
	property("specialized[Double]") = specialized[Double] is classOf[Double]
	property("specialized[Boolean]") = specialized[Boolean] is classOf[Boolean]
	property("specialized[Unit]") = unitProp(specialized[Unit])
	property("specialized[Any]") = specialized[Any] is classOf[AnyRef]
	property("specialized[AnyRef]") = specialized[AnyRef] is classOf[AnyRef]
	property("specialized[String]") = specialized[String] is classOf[AnyRef]

}
