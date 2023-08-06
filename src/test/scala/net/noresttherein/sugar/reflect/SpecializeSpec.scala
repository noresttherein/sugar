package net.noresttherein.sugar.reflect

import java.util.Date

import scala.reflect._
import scala.reflect.runtime.universe.TypeTag
import scala.runtime.BoxedUnit

import net.noresttherein.sugar.funny.generic
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._
import net.noresttherein.sugar.reflect.Specialize.SpecializeIndividually


object SpecializeSpec extends Properties("Specialize") {

	import RuntimeTypeSpec.{ClassComparator, RuntimeTypeProps, unitProp}


	object Implicitly extends Specialize[Array] {
		override def specialized[@specialized E: RuntimeType]: Array[E] = RuntimeType[E].newArray(1).asInstanceOf[Array[E]]
	}

	object Locally extends Specialize[RuntimeType] {
		override def specialized[@specialized E: RuntimeType]: RuntimeType[E] =  RuntimeType.specialized[E]
	}

	property("Specialize[Byte]()") =
		(Implicitly[Byte]().getClass.getComponentType is classOf[Byte]) :| "arrayType" &&
			(Locally[Byte]() is classOf[Byte])

	property("Specialize[Short]()") =
		(Implicitly[Short]().getClass.getComponentType is classOf[Short]) :| "arrayType"  &&
			(Locally[Short]() is classOf[Short])

	property("Specialize[Int]()") =
		(Implicitly[Int]().getClass.getComponentType is classOf[Int]) :| "arrayType"  &&
			(Locally[Int]() is classOf[Int])

	property("Specialize[Long]()") =
		(Implicitly[Long]().getClass.getComponentType is classOf[Long]) :| "arrayType"  &&
			(Locally[Long]() is classOf[Long])

	property("Specialize[Float]()") =
		(Implicitly[Float]().getClass.getComponentType is classOf[Float]) :| "arrayType"  &&
			(Locally[Float]() is classOf[Float])

	property("Specialize[Double]()") =
		(Implicitly[Double]().getClass.getComponentType is classOf[Double]) :| "arrayType"  &&
			(Locally[Double]() is classOf[Double])

	property("Specialize[Char]()") =
		(Implicitly[Char]().getClass.getComponentType is classOf[Char]) :| "arrayType"  &&
			(Locally[Char]() is classOf[Char])

	property("Specialize[Boolean]()") =
		(Implicitly[Boolean]().getClass.getComponentType is classOf[Boolean]) :| "arrayType"  &&
			(Locally[Boolean]() is classOf[Boolean])

	property("Specialize[Unit]()") =
		(Implicitly[Unit]().getClass.getComponentType is classOf[BoxedUnit]) :| "arrayType"  &&
			unitProp(Locally[Unit]())
	
	property("Specialize[AnyRef]()") =
		(Implicitly[AnyRef]().getClass.getComponentType is classOf[AnyRef]) :| "arrayType"  &&
			(Locally[AnyRef]() is classOf[AnyRef])
	
	property("Specialize[Any]()") =
		(Implicitly[Any]().getClass.getComponentType is classOf[AnyRef]) :| "arrayType"  &&
			(Locally[Any]() is classOf[AnyRef])
	
	property("Specialize[String]()") =
		(Implicitly[String]().getClass.getComponentType is classOf[String]) :| "arrayType"  &&
		(Locally[String]() is classOf[AnyRef])



	class RuntimeContext[@specialized T](val arg :T)(implicit val passedSpec :RuntimeType[T]) {
		def mySpec = RuntimeType.specialized[T]

		override def equals(other :Any) = other match {
			case spec :RuntimeContext[_] => arg==spec.arg && passedSpec==spec.passedSpec && mySpec==spec.mySpec
			case _ => false
		}

		override def toString = s"RuntimeContext[$mySpec]($arg)($passedSpec)"
	}

	object ParamCall extends Specialize.With[generic.Ident, RuntimeContext] {
		override def specialized[@specialized E](param: E)(implicit spec :RuntimeType[E]): RuntimeContext[E] = new RuntimeContext(param)(spec)
	}

	property("Specialize[Byte](_:Byte)") = ParamCall[Byte](42.toByte) ?= new RuntimeContext[Byte](42.toByte)
	property("Specialize[Short](_:Short)") = ParamCall[Short](42.toShort) ?= new RuntimeContext[Short](42.toShort)
	property("Specialize[Int](_:Int)") = ParamCall[Int](42.toInt) ?= new RuntimeContext[Int](42.toInt)
	property("Specialize[Long](_:Long)") = ParamCall[Long](42.toLong) ?= new RuntimeContext[Long](42.toLong)
	property("Specialize[Float](_:Float)") = ParamCall[Float](42.toFloat) ?= new RuntimeContext[Float](42.toFloat)
	property("Specialize[Double](_:Double)") = ParamCall[Double](42.toDouble) ?= new RuntimeContext[Double](42.toDouble)
	property("Specialize[Char](_:Char)") = ParamCall[Char](42.toChar) ?= new RuntimeContext[Char](42.toChar)
	property("Specialize[Any](_:Any)") = ParamCall[Any](42) ?= new RuntimeContext[Any](42)
	property("Specialize[AnyRef](_:AnyRef)") = ParamCall[AnyRef](new Date(42L)) ?= new RuntimeContext[AnyRef](new Date(42L))
	property("Specialize[String](_:String)") = (ParamCall[String]("42") :Any) ?= new RuntimeContext[String]("42")



	object AnyType extends SpecializeIndividually[Array] {
		override def forByte: Array[Byte] = new Array[Byte](1)
		override def forShort: Array[Short] = new Array[Short](1)
		override def forChar: Array[Char] = new Array[Char](1)
		override def forInt: Array[Int] = new Array[Int](1)
		override def forLong: Array[Long] = new Array[Long](1)
		override def forFloat: Array[Float] = new Array[Float](1)
		override def forDouble: Array[Double] = new Array[Double](1)
		override def forBoolean: Array[Boolean] = new Array[Boolean](1)
		override def forUnit: Array[Unit] = new Array[Unit](1)
		override def forNothing :Array[Nothing] = new Array[Nothing](1)
		override def forRef[E: RuntimeType]: Array[E] = new Array[AnyRef](1).asInstanceOf[Array[E]]
	}
	
	property("SpecializeIndividually[Byte]()") = AnyType[Byte]().getClass.getComponentType is classOf[Byte]
	property("SpecializeIndividually[Short]()") = AnyType[Short]().getClass.getComponentType is classOf[Short]
	property("SpecializeIndividually[Int]()") = AnyType[Int]().getClass.getComponentType is classOf[Int]
	property("SpecializeIndividually[Long]()") = AnyType[Long]().getClass.getComponentType is classOf[Long]
	property("SpecializeIndividually[Float]()") = AnyType[Float]().getClass.getComponentType is classOf[Float]
	property("SpecializeIndividually[Double]()") = AnyType[Double]().getClass.getComponentType is classOf[Double]
	property("SpecializeIndividually[Char]()") = AnyType[Char]().getClass.getComponentType is classOf[Char]
	property("SpecializeIndividually[Boolean]()") = AnyType[Boolean]().getClass.getComponentType is classOf[Boolean]
	property("SpecializeIndividually[Unit]()") = AnyType[Unit]().getClass.getComponentType is classOf[BoxedUnit]
	property("SpecializeIndividually[AnyRef]()") = AnyType[AnyRef]().getClass.getComponentType is classOf[AnyRef]
	property("SpecializeIndividually[Any]()") = AnyType[Any]().getClass.getComponentType is classOf[AnyRef]
	property("SpecializeIndividually[String]()") = AnyType[String]().getClass.getComponentType is classOf[AnyRef]
	
}
