package net.noresttherein.sugar

import scala.annotation.nowarn

import org.scalacheck.{Prop, Properties}
import net.noresttherein.sugar.casting.castingMethods
import net.noresttherein.sugar.extensions.boxeqMethod
import net.noresttherein.sugar.reflect.extensions.ClassExtension




object boxeqSpec extends Properties("boxeqMethod") {
	private val string1 = "1"
	private val string2 = "2"
	private def byte1 = 1.toByte.asAnyRef
	private def byte2 = 2.toByte.asAnyRef
	private def short1 = 1.toShort.asAnyRef
	private def short2 = 2.toShort.asAnyRef
	private def char1 = '1'.asAnyRef
	private def char2 = '2'.asAnyRef
	private def int1 = 1.asAnyRef
	private def int2 = 2.asAnyRef
	private def long1 = 1L.asAnyRef
	private def long2 = 2L.asAnyRef
	private def float1 = 1.0f.asAnyRef
	private def float2 = 2.0f.asAnyRef
	private def double1 = 1.0.asAnyRef
	private def double2 = 2.0.asAnyRef

	def shouldEqual(v1 :Any, v2 :Any) :Prop =
		Prop(v1 boxeq v2) label s"($v1 :${v1.getClass.name}) != ($v2 :${v2.getClass.name})"

	def shouldNotEqual(v1 :Any, v2 :Any) :Prop =
		Prop(!(v1 boxeq v2)) label s"($v1 :${v1.getClass.name}) == ($v2 :${v2.getClass.name})"

	property("boxeq[Unit]") = shouldEqual((), ())

	property("boxeq[Boolean]") = shouldEqual(true, true) && shouldNotEqual(true, false)

	property("boxeq[Byte]") = 
		shouldEqual(byte1, 1.toByte) && shouldNotEqual(byte1, byte2) &&
			shouldNotEqual(byte1, short1) && shouldNotEqual(byte1, int1) && shouldNotEqual(byte1, long1) &&
			shouldNotEqual(byte1, float1) && shouldNotEqual(byte1, double1)

	property("boxeq[Short]") = 
		shouldEqual(short1, 1.toShort) && shouldNotEqual(short1, short2) &&
			shouldNotEqual(short1, byte1) && shouldNotEqual(short1, int1) && shouldNotEqual(short1, long1) &&
			shouldNotEqual(short1, float1) && shouldNotEqual(short1, double1)

	property("boxeq[Char]") = 
		shouldEqual(char1, new Character('1') : @nowarn) && shouldNotEqual(char1, char2) &&
			shouldNotEqual(char1, byte1) && shouldNotEqual(char1, int1) && shouldNotEqual(char1, long1) &&
			shouldNotEqual(char1, float1) && shouldNotEqual(char1, double1)

	property("boxeq[Int]") = 
		shouldEqual(int1, 1) && shouldNotEqual(int1, int2) &&
			shouldNotEqual(int1, byte1) && shouldNotEqual(int1, short1) && shouldNotEqual(int1, long1) &&
			shouldNotEqual(int1, float1) && shouldNotEqual(int1, double1)

	property("boxeq[Long]") =
		shouldEqual(long1, 1L) && shouldNotEqual(long1, long2) &&
			shouldNotEqual(long1, byte1) && shouldNotEqual(long1, short1) && shouldNotEqual(long1, int1)
			shouldNotEqual(long1, float1) && shouldNotEqual(long1, double1)

	property("boxeq[Float]") =
		shouldEqual(float1, 1.0f) && shouldNotEqual(float1, float2) &&
			shouldNotEqual(float1, byte1) && shouldNotEqual(float1, short1) && shouldNotEqual(float1, int1) && 
			shouldNotEqual(float1, long1) && shouldNotEqual(float1, double1)

	property("boxeq[Double]") =
		shouldEqual(double1, 1.0) && shouldNotEqual(double1, double2) &&
			shouldNotEqual(double1, byte1) && shouldNotEqual(double1, short1) && shouldNotEqual(double1, int1) && 
			shouldNotEqual(double1, long1) && shouldNotEqual(double1, float1)

	property("boxeq[String]") =
		shouldEqual(string1, string1) && shouldNotEqual(string1, int1.toString) && shouldNotEqual(string1, string2)
}
