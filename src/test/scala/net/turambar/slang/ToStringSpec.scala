package net.turambar.slang

import org.scalacheck.Properties
import org.scalacheck.Prop._
import net.turambar.slang.prettyprint.ToString.DefToString
import net.turambar.slang.prettyprint.{CaseClass, ToString}

/**
  * @author Marcin Mościcki
  */
object ToStringSpec extends Properties("ToString") {
	import ToString.typePrinter

	case class Subject(boolean :Boolean, int :Int, string :String) extends CaseClass[Subject]

	val subject = new Subject(true, 42, "boggle")

	class Test extends DefToString[Test] {
		val test = 44
	}

	class CustomClass(val caliber :String, val rateOfFire :Int) {
		override def toString = this.fieldsString
	}

	property("CaseClass") = subject.toString ?= "Subject(boolean=yes, int=42, string=boggle)"

	property("DefToString") = (new Test).toString ?= "Test(test=44)"

	property("ToString") = new CustomClass("7.62", 600).toString ?= "CustomClass(caliber=7.62, rateOfFire=600)"



}
