package net.noresttherein.sugar

import org.scalacheck.{Prop, Properties}

import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.extensions.boxeqMethod
import net.noresttherein.sugar.reflect.extensions.ClassExtension




/**
  * @author Marcin Mościcki
  */
object boxeqSpec extends Properties("boxeqMethods") {
	private val object1 = new AnyRef
	private val object2 = new AnyRef
	private def byte1 = 1.toByte.asAnyRef
	private def byte2 = 2.toByte.asAnyRef
	private def short1 = 1.toShort.asAnyRef
	private def short2 = 2.toShort.asAnyRef
	private def char1 = '1'.asAnyRef
	private def char2 = '2'.asAnyRef
	private def int1 = 1.asAnyRef
	private def int2 = 2.asAnyRef
	private def long1 = 1L.asAnyRef
	private def long2= 2L.asAnyRef
	private def float1 = 1.0f.asAnyRef
	private def float2 = 2.0f.asAnyRef
	private def double1 = 1.0.asAnyRef
	private def double2 = 2.0.asAnyRef
	private def bool1 = true.asAnyRef
	private def bool2 = true.asAnyRef

	def shouldEqual(v1 :AnyRef, v2 :AnyRef) :Prop =
		Prop(v1 boxeq v2) label s"($v1 :${v1.getClass.name}) == ($v2 :${v2.getClass.name})"

	def shouldNotEqual(v1 :AnyRef, v2 :AnyRef) :Prop =
		Prop(!(v1 boxeq v2)) label s"($v1 :${v1.getClass.name}) != ($v2 :${v2.getClass.name})"

	property("boxeq[Byte]") = 
		shouldEqual(byte1, byte1) && shouldNotEqual(byte1, byte2) &&
			shouldNotEqual(short1, byte1) && shouldNotEqual(int1, byte1) && shouldNotEqual(long1, byte1) &&
			shouldNotEqual(float1, byte1) && shouldNotEqual(double1, byte1)
}
