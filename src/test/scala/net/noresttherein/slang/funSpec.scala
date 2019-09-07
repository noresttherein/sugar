package net.noresttherein.slang

import net.noresttherein.slang.funny.fun
import net.noresttherein.slang.funny.fun.ComposableFun
import net.noresttherein.slang.numeric.{LongRatio, Ratio}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop._

/**
  * @author Marcin Mościcki
  */
object funSpec extends Properties("fun") {
	import net.noresttherein.slang.optional._


	property("named(name) { ... }") = {
		val f = fun.named("square") { x :Int => x * x }
		(f.toString ?= "square") && (f(2) ?= 4)
	}


	property("name(name) { ... }") = {
		val f = fun.name("square") { x :Int => x * x }
		(f.toString ?= "square") && (f(2) ?= 4)
	}


	property("apply { ... }") =
		{
			val odd = fun { x :Int => x % 2 == 0 }
			(odd.toString ?= "Int=>Boolean") && odd(2) label "Int=>Boolean"
		} && {
			val string = fun { x :Int => x.toString }
			(string.toString ?= "_=>j.l.String") && (string(42) ?= "42") label "Int=>String"
		} && {
			trait F extends ComposableFun[Int, Long]
			val inherited :F = { x :Int => x.toLong }
			(inherited.toString ?= "Int=>Long") label "inherited"
		} && {
			trait Format extends ComposableFun[Any, String]
			trait Parser extends ComposableFun[String, Any]
			class Narrowed extends ComposableFun[String, Any] with Format with Parser {
				override def apply(v1 :Any) :String = v1.toString
			}
			((new Narrowed).toString ?= "_=>j.l.String") label "narrowed"
		} && {
			import prettyprint.shortNameOf
			abstract class Rename[P1, P2] extends ComposableFun[P1, P2]
			abstract class Swap[R, A] extends Rename[A, R]
			val erased :Swap[LongRatio, Ratio] = { x :Ratio => x }
			(erased.toString ?= s"${shortNameOf[Ratio]}=>${shortNameOf[LongRatio]}") label "transitive"
		}




}
