package net.turambar.slang

import org.scalacheck.{Prop, Properties}
import org.scalatest.exceptions.TestFailedException

/**
  * @author Marcin Mościcki
  */
object OptionalSpec extends Properties("optional") {
	import optional._


	property("Conditional.?=") =
		Prop(((1==1) ?= 2 /= { throw new Exception("'false' value evaluated for a true condition") }) == 2) :| "returns first value on true" &&
		Prop(((1!=1) ?= { throw new Exception("'true' value evaluated for a false condition") } /= 4) == 4) :| "returns second value on false"

	property("Conditional.?!=") =
		Prop({
			var ifTrue, ifFalse = false
			val res = 1==1 ?!= {ifTrue = true; 2} /!= {ifFalse = true; 4}
			res==2 && ifTrue && ifFalse
		}) :| "evaluates both alternatives on true" &&
		Prop({
			var ifTrue, ifFalse = false
			val res = 1!=1 ?!= {ifTrue = true; 2} /!= {ifFalse = true; 4}
			res==4 && ifTrue && ifFalse
		}) :| "evaluates both alternatives on false"


//	var x = 1; x = 1==1 ?= 2 /= 3
//	var y = 1; y = 1==1 ifTrue 2 orElse 3
}
