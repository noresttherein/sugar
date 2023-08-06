package net.noresttherein.sugar.reflect.prettyprint

import scala.collection.AbstractSeq
import scala.reflect.ClassTag

import net.noresttherein.sugar.JavaTypes.{JBoolean, JByte, JChar, JDouble, JFloat, JInt, JLong, JShort}
import org.scalacheck.Prop._
import org.scalacheck.Properties
import net.noresttherein.sugar.extensions.classNameMethods





object ClassNameMethodsSpec extends Properties("classNameMethods") {
	class Nest {
		class !@#%^&*-+=<>?/|\:

		val anonymous = new AnyRef {}

		object singleton
	}
	val nest = new Nest; import nest._

	class Spec[@specialized T, @specialized S] {
		class boo
//		class shoo(t :T, s :S)
	}
	trait Outer {
		val inner :Inner
		val anon  :Inner
	}
	trait Inner

	val Outer = new Outer {
		class SubInner extends Inner
		override val inner = new SubInner
		override val anon  = new Inner {}
	}

	val ByteShort = new Spec[Byte, Short]
	val IntInt = new Spec[Int, Int]
	val LongInt = new Spec[Long, Int]
	val FloatUnit = new Spec[Float, Unit]
	val DoubleUnit = new Spec[Double, Unit]
	val CharBool = new Spec[Char, Boolean]

	def primitivesProp(name :Class[_] => String) = all(
		name(classOf[Byte]) ?= "Byte",
		name(classOf[Short]) ?= "Short",
		name(classOf[Int]) ?= "Int",
		name(classOf[Long]) ?= "Long",
		name(classOf[Float]) ?= "Float",
		name(classOf[Double]) ?= "Double",
		name(classOf[Char]) ?= "Char",
		name(classOf[Boolean]) ?= "Boolean",
	)

	def arrayProp[X :ClassTag](expectedName :String)(name :Class[_] => String) =
		name(Array.ofDim[X](1, 2, 3).getClass) ?= s"Array[Array[Array[$expectedName]]]"

	def arraysProp(prefix :String, name :Class[_] => String) = all(
		arrayProp[Byte]("Byte")(name),
		arrayProp[Short]("Short")(name),
		arrayProp[Int]("Int")(name),
		arrayProp[Long]("Long")(name),
		arrayProp[Double]("Double")(name),
		arrayProp[Float]("Float")(name),
		arrayProp[Char]("Char")(name),
		arrayProp[Boolean]("Boolean")(name),
		arrayProp[Unit]("Unit")(name),
		arrayProp[Any]("AnyRef")(name),
		arrayProp[JByte](prefix + "Byte")(name),
		arrayProp[JShort](prefix + "Short")(name),
		arrayProp[JInt](prefix + (if (prefix.length > 1) "Integer" else "Int"))(name),
		arrayProp[JLong](prefix + "Long")(name),
		arrayProp[JDouble](prefix + "Double")(name),
		arrayProp[JFloat](prefix + "Float")(name),
		arrayProp[JChar](prefix + ((if (prefix.length > 1) "Character" else "Char")))(name),
		arrayProp[JBoolean](prefix + "Boolean")(name),
	)

	
	property("innerClassName") = {
		((new !@#%^&*-+=<>?/|\:).innerClassName ?= "!@#%^&*-+=<>?/|\\:") &&
			(anonymous.innerClassName ?= "Nest.anon") &&
			(singleton.innerClassName ?= "singleton") &&
			(ByteShort.innerClassName ?= "Spec[Byte,Short]") &&
			(IntInt.innerClassName ?= "Spec[Int,Int]") &&
			(LongInt.innerClassName ?= "Spec[Long,Int]") &&
			(FloatUnit.innerClassName ?= "Spec[Float,Unit]") &&
			(DoubleUnit.innerClassName ?= "Spec[Double,Unit]")  &&
			(CharBool.innerClassName ?= "Spec[Char,Boolean]") &&
			((new DoubleUnit.boo).innerClassName ?= "boo") &&
			((new AnyRef).innerClassName ?= "AnyRef") &&
			primitivesProp(innerNameOf) &&
			arraysProp("J", innerNameOf)
	}

	property("localClassName") = {
		((new !@#%^&*-+=<>?/|\:).localClassName ?= "ClassNameMethodsSpec.Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.localClassName ?= "ClassNameMethodsSpec.Nest.anon.1") &&
			(singleton.localClassName ?= "ClassNameMethodsSpec.Nest.singleton") &&
			(ByteShort.localClassName ?= "ClassNameMethodsSpec.Spec[Byte,Short]") &&
			(IntInt.localClassName ?= "ClassNameMethodsSpec.Spec[Int,Int]") &&
			(LongInt.localClassName ?= "ClassNameMethodsSpec.Spec[Long,Int]") &&
			(FloatUnit.localClassName ?= "ClassNameMethodsSpec.Spec[Float,Unit]") &&
			(DoubleUnit.localClassName ?= "ClassNameMethodsSpec.Spec[Double,Unit]")  &&
			(CharBool.localClassName ?= "ClassNameMethodsSpec.Spec[Char,Boolean]") &&
			((new DoubleUnit.boo).localClassName ?= "ClassNameMethodsSpec.Spec.boo") &&
			((new AnyRef).localClassName ?= "AnyRef") &&
			primitivesProp(localNameOf) &&
			arraysProp("J", localNameOf)
	}

	property("abbrevClassName") = {
		val prefix = "n.n.s.r.p.ClassNameMethodsSpec"
		((new !@#%^&*-+=<>?/|\:).abbrevClassName ?= prefix + ".Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.abbrevClassName ?= prefix + ".Nest.anon.1") &&
			(singleton.abbrevClassName ?= prefix + ".Nest.singleton") &&
			(ByteShort.abbrevClassName ?= prefix + ".Spec[Byte,Short]") &&
			(IntInt.abbrevClassName ?= prefix + ".Spec[Int,Int]") &&
			(LongInt.abbrevClassName ?= prefix + ".Spec[Long,Int]") &&
			(FloatUnit.abbrevClassName ?= prefix + ".Spec[Float,Unit]") &&
			(DoubleUnit.abbrevClassName ?= prefix + ".Spec[Double,Unit]")  &&
			(CharBool.abbrevClassName ?= prefix + ".Spec[Char,Boolean]") &&
			((new DoubleUnit.boo).abbrevClassName ?= prefix + ".Spec.boo") &&
			((new AnyRef).abbrevClassName ?= "AnyRef") &&
			primitivesProp(abbrevNameOf) &&
			arraysProp("j.l.", abbrevNameOf)
	}

	property("className") = {
		val prefix = getClass.getName.substring(0, getClass.getName.length - 1)
		((new !@#%^&*-+=<>?/|\:).className ?= prefix + ".Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.className ?= prefix +".Nest.anon.1") &&
			(singleton.className ?= prefix + ".Nest.singleton") &&
			(ByteShort.className ?= prefix + ".Spec[Byte,Short]") &&
			(IntInt.className ?= prefix + ".Spec[Int,Int]") &&
			(LongInt.className ?= prefix + ".Spec[Long,Int]") &&
			(FloatUnit.className ?= prefix + ".Spec[Float,Unit]") &&
			(DoubleUnit.className ?= prefix + ".Spec[Double,Unit]")  &&
			(CharBool.className ?= prefix + ".Spec[Char,Boolean]") &&
			((new DoubleUnit.boo).className ?= prefix + ".Spec.boo") &&
			((new AnyRef).className ?= "AnyRef") &&
			primitivesProp(fullNameOf) &&
			arraysProp("java.lang.", fullNameOf)
	}


	Console.err.println(ByteShort.getClass.getName)
	Console.err.println(Outer.inner.getClass.getName)
	Console.err.println(Outer.anon.getClass.getName)
	Console.err.println(classOf[Inner].getName)
	Console.err.println((new ClassNameMethodsSpec).inner.getClass.getName)
	Console.err.println((new ClassNameMethodsSpec).anon.getClass.getName)
	Console.err.println((new !@#%^&*-+=<>?/|\:).getClass.getName)
	Console.err.println(Array.ofDim[Any](1, 1, 1).getClass.getName)
}





private class ClassNameMethodsSpec {
	class InnerClass
	val inner = new InnerClass
	val anon = new AnyRef {}
//	var lambda :In
}

