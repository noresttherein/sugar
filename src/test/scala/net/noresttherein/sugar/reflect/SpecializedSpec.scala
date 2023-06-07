package net.noresttherein.sugar.reflect

import scala.reflect._
import scala.reflect.runtime.universe.TypeTag

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._

import net.noresttherein.sugar.reflect.RuntimeType.Specialized
import net.noresttherein.sugar.reflect.RuntimeType.Specialized




object SpecializedSpec extends Properties("Specialized") {
	
	import RuntimeTypeSpec.{RuntimeTypeProps, unitProp}
	
	/** Implicit accessor with explicit types. */
	
	property("apply[Byte]") = Specialized[Byte] is classOf[Byte]
	property("apply[Short]") = Specialized[Short] is classOf[Short]
	property("apply[Int]") = Specialized[Int] is classOf[Int]
	property("apply[Long]") = Specialized[Long] is classOf[Long]
	property("apply[Char]") = Specialized[Char] is classOf[Char]
	property("apply[Float]") = Specialized[Float] is classOf[Float]
	property("apply[Double]") = Specialized[Double] is classOf[Double]
	property("apply[Boolean]") = Specialized[Boolean] is classOf[Boolean]
	property("apply[Unit]") = unitProp(Specialized[Unit])
	property("apply[Any]") = Specialized[Any] is classOf[AnyRef]
	property("apply[AnyRef]") = Specialized[AnyRef] is classOf[AnyRef]
	property("apply[String]") = Specialized[String] is classOf[AnyRef]



	def specialized[@specialized T] :Specialized[T] = Specialized[T]

	property("apply[@specialized T]") = {
		import SpecializedSpec.{specialized => spec}
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
		def spec[T :TypeTag] :Specialized[T] = Specialized[T]

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



	property("apply[T :ClassTag]") = {
		def spec[T :ClassTag] :Specialized[T] = Specialized[T]

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


	property("apply[T :Specialized]") = {
		def spec[T :Specialized] = Specialized[T]

		(spec(RuntimeType.OfByte) is classOf[Byte]) &&
			(spec(RuntimeType.erased[Any]) is classOf[AnyRef])
	}


	def specializedWithTypeClass[@specialized T :Specialized] = Specialized[T]

	property("apply[@specialized T :Specialized]") = {
		import RuntimeTypeSpec.{specializedWithTypeClass => spec}
		(spec[Byte] is classOf[Byte]) &&
			(spec[Any] is classOf[AnyRef]) &&
			(spec[Byte](RuntimeType.erased) is classOf[AnyRef]) :| "type class should take precedence over specialization"
	}



}
