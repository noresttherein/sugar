import java.math.{MathContext, RoundingMode}
import java.math.MathContext.{DECIMAL128, DECIMAL32}
import java.math.RoundingMode.{DOWN, FLOOR, HALF_DOWN, HALF_EVEN, HALF_UP, UNNECESSARY, UP}

import scala.math.BigDecimal.RoundingMode.CEILING

import net.noresttherein.sugar.numeric.{Decimal64, Decimal64Spec}
import net.noresttherein.sugar.numeric.Decimal64.Round.{maxPrecision, Extended, ExtendedExact, Standard}
import net.noresttherein.sugar.numeric.Decimal64.implicits.scientificDecimalNotation
import net.noresttherein.sugar.numeric.Decimal64.{Precision, Round}
import net.noresttherein.sugar.vars.Opt




object Playground extends App {

	trait Box {
		type Bound[-T] >: Contra[T]
		type Contra[-T]
		def box :Box { type Contra[-T] <: Bound[T] }
	}

	trait High[T[_]]

	trait Sub[T[-_]] extends High[T]

	type Split[X, Y, Z] = (X => Y, Y => Z)
	def split[X, Z](f :X => Z) :Opt[(X => Y, Y => Z)] forSome { type Y } = ???

	split((_:Int).toString) match {
		case split :Opt[Split[Int, a, String]] if split.isDefined =>
			val (_1, _2) = split.get
			_1 andThen _2
			split.get._1 andThen split.get._2
	}
//	_1 :Nothing
//	_2 :Nothing
//	_1 andThen _2
/*

		trait Test[X, E[_], -A <: E[X]]
		type TT[X, -E[_], -A <: E[X]]

//	type Visit[V, U, +EC[v] <: S[V], +E <: EC[V], LeftRes, RightRes] = {
//		type Transform[-_, Y, Cons[v] >: EC[Y], Expr >: E <: Cons[Y]] =
//			Trans[V, U, Cons, Expr, RightRes] => (LeftRes, RightRes)
//	}
	trait ExprTemp[V, +EC[v] <: Expr[v], +E <: EC[V]] { this :E =>
		def self :E = this
//		def reform[EC_^[v] >: EC[v] <: S[v], ]
		def reform[X, Y, EC2[v] <: Expr[v], E2 <: EC2[X], LRes, RRes]
		          (other :ExprTemp[X, EC2, E2])
		          (t1 :Trans[V, Y, EC, E, LRes], t2 :Trans[X, Y, EC2, E2, RRes]) :(LRes, RRes) = ???
//			new Transformer[V, X, Y, EC, E, EC2, E2, LRes, RRes](this, other.self)(t1).apply(other)(t2)

		def visit[EC1[v] >: EC[v] <: Expr[v], E1 >: E <: EC1[V], Res[X, -ec[v] <: Expr[v], -e]]
		         (visitor :Visitor[Res]) :Res[V, EC1, E1]

		def visit[EC2[v] >: EC[v] <: Expr[v], E2 >: E <: EC2[V], P[v, -ec[vv] <: Expr[vv], -e <: ec[v]], R[_]]
		         (visitor :ParamVisitor[R], param :P[V, EC, E]) :R[V]
//			visitor.t[V, EC]
	}
	trait Expr[V] extends ExprTemp[V, Expr, Expr[V]] {
		override def visit[EC[v] >: Expr[v] <: Expr[v], E >: Expr[V] <: EC[V], Res[X, -ec[v] <: Expr[v], -e]]
		                  (visitor :Visitor[Res]) :Res[V, EC, E] =
			visitor.e(this)

		override def visit[EC2[v] >: Expr[v] <: Expr[v], E2 >: Expr[V] <: EC2[V], P[v, +ec[vv] <: Expr[vv], +e <: ec[v]], R[_]]
		                  (visitor :ParamVisitor[R], param :P[V, EC2, E2]) :R[V] =
			visitor.e(this, param)
//			visitor.e(this)
	}

	class Sub[V] extends Expr[V] with ExprTemp[V, Sub, Sub[V]] {
//		override def reform[X, Y, EC2[v] <: Expr[v], E2 <: EC2[X], LRes, RRes]
//		                   (other :ExprTemp[X, EC2, E2])
//		                   (t1 :Trans[V, Y, Sub, Sub[V], LRes], t2 :Trans[X, Y, EC2, E2, RRes]) :(LRes, RRes)

		override def visit[EC[v] >: Sub[v] <: Expr[v], E >: Sub[V] <: EC[V], Res[X, -ec[v] <: Expr[v], -e]]
		                  (visitor :Visitor[Res]) :Res[V, EC, E] =
			visitor.sub(this)

		override def visit[EC2[v] >: Sub[v] <: Expr[v], E2 >: Sub[V] <: EC2[V], P[v, +ec[vv] <: Expr[vv], +e <: ec[v]], R[_]]
		                  (visitor :ParamVisitor[R], param :P[V, EC2, E2]) :R[V] =
			visitor.sub[V, E2, P](this, param)
	}

	trait Trans[V, U, -EC[_], -A, +R] {
		def result(right :EC[U]) :Opt[R]
		def apply(arg :A) :R
	}

	trait Trans2[V, U, +EC[_], +R] {
		def result[ec[v] >: EC[v]](right :ec[U]) :Opt[R]
//		def apply
	}

//	trait Result[X, Y, -EC[v] <: Expr[v], -E, LRes, RRes] {
//		def apply[EC1[v] <: EC[v], E1 <: E, Res1 <: RRes](trans :Trans[X, Y, UEC forSome { type UEC[v] >: EC[v] }, _ >: E, Res1]) :(LRes, RRes)
//	}

	abstract class ParamVisitor[+Res[v]]
//	                           (param :P[V, EC, E])
	{
		def apply[V, EC[v] <: Expr[v], E <: EC[V], P[v, +EC2[vv] <: Expr[vv], +E2 <: EC2[v]]]
		         (e :ExprTemp[V, EC, E], param :P[V, EC, E]) :Res[V] = e.visit(this, param)

		def e[V, E <: Expr[V], P[v, +EC2[vv] <: Expr[vv], +E2 <: EC2[v]]]
		     (e :E, param :P[V, Expr, E]) :Res[V]

		def sub[V, E <: Sub[V], P[v, +EC2[vv] <: Expr[vv], +E2 <: EC2[v]]]
		       (e :E, param :P[V, Sub, E]) :Res[V] =
			this.e[V, Expr[V], P](e, param)
	}

	trait Visitor[+Res[V, -EC[v] <: Expr[v], -E]] {//<: EC[V]]] {
		def apply[V, EC[v] <: Expr[v], E <: EC[V]](e :ExprTemp[V, EC, E]) :Res[V, EC, E] =
			e.visit(this)

		def e[V, EC[v] >: Expr[v] <: Expr[v], E <: EC[V]](e :E) :Res[V, EC, E]
		def sub[V, EC[v] >: Sub[v] <: Expr[v], E <: EC[V]](e :E) :Res[V, EC, E] = this.e[V, Expr, Expr[V]](e)
	}

	class Transformer[L, R, Y, LEC[v] <: Expr[v], LE <: LEC[L], REC[v] <: Expr[v], RE <: REC[R], LeftRes, RightRes]
		extends Visitor[({ type Res[v, -ec[vv] <: Expr[vv], -e] = Trans[v, Y, ec, e, RightRes] => RightRes })#Res]
	{
		override def e[V, EC[v] >: Expr[v] <: Expr[v], E <: EC[V]](e :E) = ???

		override def sub[V, EC[v] >: Sub[v] <: Expr[v], E <: EC[V]](e :E) =
	}
*/

/*
	class Transformer[L, R, Y, LEC[v] <: Expr[v], LE <: LEC[L], REC[v] <: Expr[v], RE <: REC[R], LeftRes, RightRes]
	                 (left :LE, right :RE)(leftRes :Trans[L, Y, LEC, LE, LeftRes])
		extends Visitor[({ type Res[v, -EC2[vv] <: Expr[vv], -E2] = Trans[v, Y, UEC forSome { type UEC[V] >: EC2[V] }, _ >: E2, RightRes] => (LeftRes, RightRes) })#Res]
//		extends Visitor[({ type Res[v, -EC[vv] <: Expr[vv], -E] = Result[v, Y, EC, E, LeftRes, RightRes] })#Res]
	{
		override def e[V, EC[v] >: Expr[v] <: Expr[v], E <: EC[V]](e :E)
				:Trans[V, Y, EC, E, RightRes] => (LeftRes, RightRes) =
			trans => (leftRes(left), trans(e))
//				:Result[V, Y, EC, E, LeftRes, RightRes] =
//			new Result[V, Y, EC, E, LeftRes, RightRes] {
//				override def apply[EC1[v] >: EC[v], E1 >: E, Res1 <: RightRes]
//				                  (trans :Trans[V, Y, EC1, E1, Res1]) :(LeftRes, RightRes) =
//					(leftRes(left), trans(e))
//			}
		override def sub[V, EC[v] >: Sub[v] <: Expr[v], E <: EC[V]](s :E)
				:Trans[V, Y, EC, E, RightRes] => (LeftRes, RightRes) =
			trans => (leftRes(left), trans.result(new Sub[Y]).get)
//			this.e[V, EC, E](s)
//				:Result[V, Y, EC, E, LeftRes, RightRes] =
//			new Result[V, Y, EC, E, LeftRes, RightRes] {
//				override def apply[EC1[v] >: EC[v], E1 >: E, Res1 <: RightRes](trans :Trans[V, Y, EC1, E1, Res1]) =
//					(leftRes(left), trans.result(new Sub[Y]).get)
//			}
	}
*/
}
