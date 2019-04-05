package net.noresttherein.slang

import org.scalacheck.{Prop, Properties}

/**
  * @author Marcin Mościcki
  */
object OptionalSpec extends Properties("optional") {
	import net.noresttherein.slang.optional._


	property("IfTrue.ifTrue") =
		Prop(((1==1) ifTrue 2 getOrElse { throw new Exception("'false' value evaluated for a true condition") }) == 2) :| "returns first value on true" &&
		Prop(((1!=1) ifTrue { throw new Exception("'true' value evaluated for a false condition") } getOrElse 4) == 4) :| "returns second value on false"

}
