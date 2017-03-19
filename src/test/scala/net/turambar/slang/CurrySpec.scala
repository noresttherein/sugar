package net.turambar.slang

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import net.turambar.slang.funny.Curry
import net.turambar.slang.funny.Curry.Curried
import net.turambar.slang.funny.Curry.Curried.__

/**
  * @author Marcin Mościcki
  */
object CurrySpec extends Properties("Curry") {

	type F = Byte=>Short=>Int=>Long=>String
	val f :F = { b :Byte => s :Short => i :Int => l :Long => s" $b $s $i $l " }
	val g :F = { b :Byte => s :Short => i :Int => l :Long => s" $l $i $s $b " }
	val e :F = { b :Byte => s :Short => i :Int => l :Long => s" ${-b} ${-s} ${-i} ${-l} " }
	type Args = (Byte, Short, Int, Long)

	val a = { a1 :Any => a2 :Any => a3 :Any => a4 :Any => s"$a1 $a2 $a3 $a4 " }
	val b = { b1 :Any => b2 :Any => b3 :Any => b4 :Any => s"$b4 $b3 $b2 $b1 " }

	def f(args :Args) :String = f(args._1)(args._2)(args._3)(args._4)
	def g(args :Args) :String = g(args._1)(args._2)(args._3)(args._4)
	def e(args :Args) :String = e(args._1)(args._2)(args._3)(args._4)

	import Curry.PartiallyApplied.CurryOn
	import Curry.:*:

	property(":*:") = {
		def apply(f :F)(implicit args :Args) :String = { import args._; f(_1)(_2)(_3)(_4) }

		def res(implicit args :Args) = { import args._;  apply(a)(args) + apply(b)(args) }

		forAll { implicit args: Args => import args._
			(a :*: b) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Any=>String :*: Any=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			((a: (Byte => Any => Any => Any => String)) :*: b) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Byte=>Any=>Any=>Any=>String :*: Any=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			((a :Any=>Short=>Any=>Any=>String) :*: b) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Short=>Any=>Any=>String :*: Any=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			((a :Any=>Any=>Int=>Any=>String) :*: b) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Int=>Any=>String :*: Any=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			((a :Any=>Any=>Any=>Long=>String) :*: b) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Long=>String :*: Any=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			(a :*: (b :Byte=>Any=>Any=>Any=>String)) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Any=>String :*: Byte=>Any=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			(a :*: (b :Any=>Short=>Any=>Any=>String)) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Any=>String :*: Any=>Short=>Any=>Any=>String" &&
		forAll { implicit args: Args => import args._
			(a :*: (b :Any=>Any=>Int=>Any=>String)) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Any=>String :*: Any=>Any=>Int=>Any=>String" &&
		forAll { implicit args: Args => import args._
			(a :*: (b :Any=>Any=>Any=>Long=>String)) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Any=>Any=>Any=>Any=>String :*: Any=>Any=>Any=>Long=>String" &&
		forAll { implicit args: Args => import args._
			((a :Byte=>Any=>Int=>Long=>String) :*: (b :Byte=>Short=>Int=>Any=>String)) (_ + _)(_1)(_2)(_3)(_4) ?= res
		} :| "Byte=>Any=>Int=>Long=>String :*: Byte=>Short=>Int=>Any=>String"

	}



	property("Curried(x)") =
		forAll { args :Args => Curried(f)(-1.toByte).unapplied(args._2)(args._3)(args._4) ?= f(-1.toByte)(args._2)(args._3)(args._4) } :| "f(x)()()()" &&
		forAll { args :Args => Curried(f)().applied(-1.toShort)(args._1)(args._3)(args._4) ?= f(args._1)(-1.toShort)(args._3)(args._4) } :| "f()(x)()()" &&
		forAll { args :Args => Curried(f)()()(-1).unapplied(args._1)(args._2)(args._4) ?= f(args._1)(args._2)(-1)(args._4) } :| "f()()(x)()" &&
		forAll { args :Args => Curried(f)()()().applied(-1L)(args._1)(args._2)(args._3) ?= f(args._1)(args._2)(args._3)(-1) } :| "f()(x)()()"


	property("Curried.compose") =
		forAll { args :Args => import args._; Curried(f).compose((_:String).toByte).unapplied(_1.toString)(_2)(_3)(_4) ?= f(args) } :| "compose(x)()()()" &&
		forAll { args :Args => import args._; Curried(f)().composed((_:String).toShort)(_1)(_2.toString)(_3)(_4) ?= f(args) } :| "compose()(x)()()" &&
		forAll { args :Args => import args._; Curried(f)()()((_:String).toInt).unapplied(_1)(_2)(_3.toString)(_4) ?= f(args) } :| "compose()()(x)()" &&
		forAll { args :Args => import args._; Curried(f)()()().compose((_:String).toLong).unapplied(_1)(_2)(_3)(_4.toString) ?= f(args) } :| "compose()()()(x)"
		forAll { args :Args => import args._; Curried(f)((_:Int).toByte)((_:Int).toShort)().composed((_:Int).toLong)(_1.toInt)(_2.toInt)(_3)(_4.toInt) ?= f(args) } :| "compose()()()(x)"

	property("Curried.mapped") =
		forAll { args :Args => import args._; Curried(f).mapped(g => (a :Args) => g(a._1)(a._2)(a._3)(a._4))(args) ?= f(args) } :| "mapped{}" &&
		forAll { args :Args => import args._; Curried(f)().mapped(g => (s :Short, i :Int, l :Long) => g(s)(i)(l))(_1)(_2, _3, _4) ?= f(args) } :| "().mapped{}" &&
		forAll { args :Args => import args._; Curried(f)()().mapped(g => (i :Int, l :Long) => g(i)(l))(_1)(_2)(_3, _4) ?= f(args) } :| "()().mapped{}" &&
		forAll { args :Args => import args._; Curried(f)()()().mapped(g => (l :Long) => g(l) + g(l))(_1)(_2)(_3)(_4) ?= (f(args) + f(args))} :| "()()().mapped{}"

	property("Curried.accept") =
		forAll { args :Args => import args._; Curried(f)(__[String]).unapplied("spy")(_1)(_2)(_3)(_4) ?= f(args)} :| "(__)" &&
		forAll { args :Args => import args._; Curried(f)()(__[String]).unapplied(_1)("spy")(_2)(_3)(_4) ?= f(args)} :| "()(__)" &&
		forAll { args :Args => import args._; Curried(f)()().accept[String]((_:Unit)=>"spy").unapplied(_1)(_2)(())(_3)(_4) ?= f(args)} :| "()().take" &&
		forAll { args :Args => import args._; Curried(f)()()().accepting[String](_1)(_2)(_3)("spy")(_4) ?= f(args)} :| "()()().accepting"

	property("Curried.combined") =
		forAll { args :Args => import args._
			Curried(f).combined[Byte, Short=>Int=>Long=>String, Args=>String](g){ (l, r) => (a :Args) => l(a._1)(a._2)(a._3)(a._4) + r(a._1)(a._2)(a._3)(a._4) }(args) ?= (f(args) + g(args))
		} :| "combine{}" &&
		forAll { args :Args => import args._
			Curried(f).next.combined[Short, Int=>Long=>String, Args=>String](g){ (l, r) => (a :Args) => l(a._2)(a._3)(a._4) + r(a._2)(a._3)(a._4) }(_1)(args) ?= (f(args) + g(args))
		} :| "combine{}" &&
		forAll { args :Args => import args._
			Curried(f)()().combined[Int, Long=>String, Args=>String](g){ (l, r) => (a :Args) => l(a._3)(a._4) + r(a._3)(a._4) }(_1)(_2)(args) ?= (f(args) + g(args))
		} :| "combine{}" &&
		forAll { args :Args => import args._
			Curried(f).__.__.__.combined[Long, String, Long=>String](g){ (l, r) => (a :Long) => l(a) + r(a) }(_1)(_2)(_3)(_4) ?= (f(args) + g(args))
		} :| "combine{}"

	property("Curried.last") = forAll { a :Args => import a._
		Curried.last(f).applied(_4)(_1)(_2)(_3) ?= f(a)
	}



	property("Curried.*") =
		forAll { args :Args =>
			(Curried(f) * Curried(g)){ (l, r) => (a :Args) => import a._; l(_1)(_2)(_3)(_4) + r(_1)(_2)(_3)(_4) }(args) ?= (f(args) + g(args))
		} :| ":*:" &&
		forAll { args :Args => import args._
			(Curried(f)() * Curried(g)()){ (l, r) => (a :Args) => import a._; l(_2)(_3)(_4) + r(_2)(_3)(_4) }(_1)(args) ?= (f(args) + g(args))
		} :| "():*:" &&
		forAll { args :Args => import args._
			(Curried(f).__.__ * Curried(g).__.__){ (l, r) => (a :Args) => import a._; l(_3)(_4) + r(_3)(_4) }(_1)(_2)(args) ?= (f(args) + g(args))
		} :| "()():*:" &&
		forAll { args :Args => import args._
			(Curried(f).next.next.next * Curried(g).next.next.next){ (l, r) => (a :Long) => l(a)+r(a) }(_1)(_2)(_3)(_4) ?= (f(args) + g(args))
		} :| "()()():*:"






}
