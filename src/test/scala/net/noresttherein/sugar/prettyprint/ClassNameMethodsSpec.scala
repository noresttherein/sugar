package net.noresttherein.sugar.prettyprint

import scala.collection.AbstractSeq

import org.scalacheck.Properties
import org.scalacheck.Prop._

import net.noresttherein.sugar.extensions.classNameMethods






/**
  * @author Marcin Mościcki
  */
object ClassNameMethodsSpec extends Properties("C1lassNameMethods") {

	class Nest {
		class !@#%^&*-+=<>?/|\:

		def anonymous :Seq[Nothing] = new AbstractSeq[Nothing] with Seq[Nothing] {
			override def apply(i :Int) = ???
			override def length = 0
			override def iterator = Iterator.empty
		}

		object singleton
	}
	val nest = new Nest; import nest._

	class Spec[@specialized T, @specialized S] {
		class boo
//		class shoo(t :T, s :S)
	}
	val ByteShort = new Spec[Byte, Short]
	val IntInt = new Spec[Int, Int]
	val LongInt = new Spec[Long, Int]
	val FloatUnit = new Spec[Float, Unit]
	val DoubleUnit = new Spec[Double, Unit]
	val CharBool = new Spec[Char, Boolean]

	property("innerClassName") = {
		((new !@#%^&*-+=<>?/|\:).innerClassName ?= "!@#%^&*-+=<>?/|\\:") &&
			(anonymous.innerClassName ?= "Nest.anon") &&
			(singleton.innerClassName ?= "singleton") &&
			(ByteShort.innerClassName ?= "Spec[Byte,Short]") &&
			(IntInt.innerClassName ?= "Spec[Int,Int]") &&
			(LongInt.innerClassName ?= "Spec[Long,Int]") &&
			(FloatUnit.innerClassName ?= "Spec[Float,Unit]") &&
			(DoubleUnit.innerClassName ?= "Spec[Double,Unit]")  &&
			(CharBool.innerClassName ?= "Spec[Char,Bool]") &&
			((new DoubleUnit.boo).innerClassName ?= "boo") &&
//			((new LongInt.shoo(42, 44)).innerClassName ?= "shoo") &&
			(Array.ofDim[Int](1, 2, 3).innerClassName ?= "Array[Array[Array[Int]]]")
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
			(CharBool.localClassName ?= "ClassNameMethodsSpec.Spec[Char,Bool]") &&
			((new DoubleUnit.boo).localClassName ?= "ClassNameMethodsSpec.Spec.boo") &&
//			((new LongInt.shoo(42, 44)).localClassName ?= "classNameMethodsSpec.Spec[Long, Int].shoo") &&
			(Array.ofDim[Int](1, 2, 3).localClassName ?= "Array[Array[Array[Int]]]")

	}

	property("abbrevClassName") = {
		val prefix = "n.n.s.p.ClassNameMethodsSpec"
		((new !@#%^&*-+=<>?/|\:).abbrevClassName ?= prefix + ".Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.abbrevClassName ?= prefix + ".Nest.anon.1") &&
			(singleton.abbrevClassName ?= prefix + ".Nest.singleton") &&
			(ByteShort.abbrevClassName ?= prefix + ".Spec[Byte,Short]") &&
			(IntInt.abbrevClassName ?= prefix + ".Spec[Int,Int]") &&
			(LongInt.abbrevClassName ?= prefix + ".Spec[Long,Int]") &&
			(FloatUnit.abbrevClassName ?= prefix + ".Spec[Float,Unit]") &&
			(DoubleUnit.abbrevClassName ?= prefix + ".Spec[Double,Unit]")  &&
			(CharBool.abbrevClassName ?= prefix + ".Spec[Char,Bool]") &&
			((new DoubleUnit.boo).abbrevClassName ?= prefix + ".Spec.boo") &&
//			((new LongInt.shoo(42, 44)).abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Long,Int].shoo") &&
			(Array.ofDim[Int](1, 2, 3).abbrevClassName ?= "Array[Array[Array[Int]]]")
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
			(CharBool.className ?= prefix + ".Spec[Char,Bool]") &&
			((new DoubleUnit.boo).className ?= prefix + ".Spec.boo") &&
//			((new LongInt.shoo(42, 44)).className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Long,Int].shoo") &&
			(classNameOf(Array.ofDim[Int](1, 2, 3)) ?= "Array[Array[Array[Int]]]")
	}
}
