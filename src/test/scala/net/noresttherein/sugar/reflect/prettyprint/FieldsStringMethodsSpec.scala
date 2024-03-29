package net.noresttherein.sugar.reflect.prettyprint

import scala.annotation.nowarn

import org.scalacheck.Prop._
import org.scalacheck.Properties

//implicits
import net.noresttherein.sugar.extensions.fieldsStringMethods




/**
  * @author Marcin Mościcki
  */
object FieldsStringMethodsSpec extends Properties("fieldsStringMethods") {

	case class Subject(boolean :Boolean, int :Int, string :String) extends CaseClass[Subject]

	val subject = Subject(true, 42, "boggle")

	class Test extends DefToString[Test] {
		val test = 44
	}

	class CustomClass(val caliber :String, val rateOfFire :Int) {
		override def toString :String = this.gettersString
	}

	class Private(private val field1 :String, param :Int) {
		@nowarn private[this] var _field2 = param

		def field :Int = _field2

		override def toString = this.fieldsString
	}

	property("CaseClass") = subject.toString ?= "Subject(boolean=yes, int=42, string=boggle)"

	property("DefToString") = (new Test).toString ?= "Test(test=44)"

	property("gettersString") = new CustomClass("7.62", 600).toString ?= "CustomClass(caliber=7.62, rateOfFire=600)"

	property("fieldsString") = new Private("first", 42).toString ?= "Private(field1=first, _field2=42)"


}
