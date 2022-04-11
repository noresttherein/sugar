package net.noresttherein.sugar.fun

import net.noresttherein.sugar.funny.fun
import net.noresttherein.sugar.funny.fun.ComposableFun
import net.noresttherein.sugar.numeric.{Ratio, IntRatio}
import net.noresttherein.sugar.prettyprint
import org.scalacheck.Prop._
import org.scalacheck.Properties

/**
  * @author Marcin Mościcki
  */
object funSpec extends Properties("fun") {


	property("named(name) { ... }") = {
		val f = fun.named[Int]("square") { x => x * x }
		(f.toString ?= "square") && (f(2) ?= 4)
	}


	property("name(name) { ... }") = {
		val f = fun.name("square") { x :Int => x * x }
		(f.toString ?= "square") && (f(2) ?= 4)
	}


	property("apply { ... }") =
		{
			val odd = fun[Int] { x => x % 2 == 0 }
			(odd.toString ?= "Int=>Boolean") && odd(2) label "Int=>Boolean"
		} && {
			val string = fun[Int] { x :Int => x.toString }
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
			import prettyprint.abbrevNameOf
			abstract class Rename[P1, P2] extends ComposableFun[P1, P2]
			abstract class Swap[R, A] extends Rename[A, R]
			val erased :Swap[Ratio, IntRatio] = { x :IntRatio => x }
			(erased.toString ?= s"${abbrevNameOf[IntRatio]}=>${abbrevNameOf[Ratio]}") label "transitive"
		}




}
