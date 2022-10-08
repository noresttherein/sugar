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
		((new !@#%^&*-+=<>?/|\:).localClassName ?= "classNameMethodsSpec.Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.localClassName ?= "classNameMethodsSpec.Nest.anon.1") &&
			(singleton.localClassName ?= "classNameMethodsSpec.Nest.singleton") &&
			(ByteShort.localClassName ?= "classNameMethodsSpec.Spec[Byte,Short]") &&
			(IntInt.localClassName ?= "classNameMethodsSpec.Spec[Int,Int]") &&
			(LongInt.localClassName ?= "classNameMethodsSpec.Spec[Long,Int]") &&
			(FloatUnit.localClassName ?= "classNameMethodsSpec.Spec[Float,Unit]") &&
			(DoubleUnit.localClassName ?= "classNameMethodsSpec.Spec[Double,Unit]")  &&
			(CharBool.localClassName ?= "classNameMethodsSpec.Spec[Char,Bool]") &&
			((new DoubleUnit.boo).localClassName ?= "classNameMethodsSpec.Spec.boo") &&
//			((new LongInt.shoo(42, 44)).localClassName ?= "classNameMethodsSpec.Spec[Long, Int].shoo") &&
			(Array.ofDim[Int](1, 2, 3).localClassName ?= "Array[Array[Array[Int]]]")

	}

	property("abbrevClassName") = {
		((new !@#%^&*-+=<>?/|\:).abbrevClassName ?=
			"n.n.s.p.classNameMethodsSpec.Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Nest.anon.1") &&
			(singleton.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Nest.singleton") &&
			(ByteShort.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Byte,Short]") &&
			(IntInt.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Int,Int]") &&
			(LongInt.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Long,Int]") &&
			(FloatUnit.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Float,Unit]") &&
			(DoubleUnit.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Double,Unit]")  &&
			(CharBool.abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Char,Bool]") &&
			((new DoubleUnit.boo).abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec.boo") &&
//			((new LongInt.shoo(42, 44)).abbrevClassName ?= "n.n.s.p.classNameMethodsSpec.Spec[Long,Int].shoo") &&
			(Array.ofDim[Int](1, 2, 3).abbrevClassName ?= "Array[Array[Array[Int]]]")
	}

	property("className") = {
		((new !@#%^&*-+=<>?/|\:).className ?=
			"net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Nest.!@#%^&*-+=<>?/|\\:") &&
			(anonymous.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Nest.anon.1") &&
			(singleton.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Nest.singleton") &&
			(ByteShort.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Byte,Short]") &&
			(IntInt.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Int,Int]") &&
			(LongInt.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Long,Int]") &&
			(FloatUnit.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Float,Unit]") &&
			(DoubleUnit.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Double,Unit]")  &&
			(CharBool.className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Char,Bool]") &&
			((new DoubleUnit.boo).className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec.boo") &&
//			((new LongInt.shoo(42, 44)).className ?= "net.noresttherein.sugar.prettyprint.classNameMethodsSpec.Spec[Long,Int].shoo") &&
			(classNameOf(Array.ofDim[Int](1, 2, 3)) ?= "Array[Array[Array[Int]]]")
	}
}
