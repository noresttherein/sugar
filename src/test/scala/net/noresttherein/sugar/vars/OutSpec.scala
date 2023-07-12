package net.noresttherein.sugar.vars

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._



object OutSpec extends Properties("vars.Out") {
	property("value") =
		Prop(throws(classOf[NoSuchElementException]) { Out[Int]().value }) :| "Uninitialized access" &&
		{ val out = Out[Int](); out.value = 42; out.value ?= 42 } :| "Initialized access" &&
		Prop(throws(classOf[IllegalStateException]) {
			val out = Out[Int](); out.value = 42; out.value = 44
		}) :| "Subsequent write attempt"

	property("value<erased>") =
		Prop(throws(classOf[NoSuchElementException]) { Out[String]().value }) :| "Uninitialized access" &&
			{ val out = Out[String](); out.value = "42"; out.value ?= "42" } :| "Initialized access" &&
			Prop(throws(classOf[IllegalStateException]) {
				val out = Out[String](); out.value = "42"; out.value = "44"
			}) :| "Subsequent write attempt"

}
