
import scala.collection.immutable.{ArraySeq, ListSet, SortedMap, TreeSet}
import scala.collection.{IndexedSeqView, SortedSet}

import net.noresttherein.sugar.collections.{IndexedSet, MultiSet, OrderedItems, PassedArray, Ranking, ZigZag}
import net.noresttherein.sugar.extensions.{classExtension, classNameExtension, classNameMethods}
import net.noresttherein.sugar.reflect.BoxClass
import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.JavaTypes.JByte
import net.noresttherein.sugar.numeric
import net.noresttherein.sugar.numeric.Natural
import net.noresttherein.sugar.util.LabelPath.{/, ~/, Label}





object Playground extends App {

//	println(Ranking(0, 127489316, -1) ++ Ranking(0, -1, 539749312))
	println(Nil.startsWith(Nil, -1))
	println(Nil.startsWith(Nil, 0))
	println(Nil.startsWith(Nil, 1))

	println(IndexedSet.from(TreeSet(-1118745115, 2147483647)))

	trait Convert[X, Y] {
		type Res[+E <: Trait[Y]] <: Trait[Y]
		def apply[E[v] <: Templated[v, E]](e :Templated[X, E]) :Res[E[Y]] = ???
	}

	trait Trait[V] extends Template[V, Trait]

	trait Producer[+R] {
		def apply() :R
	}

	trait Template[V, +E[v] <: Templated[v, E]] { self :E[V] with Template[V, E] =>
		class Inner[R[+e <: Trait[U]] <: Trait[U], U]
		           (convert :Convert[V, U] { type Res[+e <: Trait[U]] = R[e] })
			extends Producer[R[E[U]]]
		{
			def apply() :R[E[U]] = convert(self)
		}
	}
	type Templated[V, +E[v] <: Templated[v, E]] = Trait[V] with Template[V, E]

	trait Intermediate[V] extends Trait[V] with IntermediateTemplate[V, Intermediate]

	trait IntermediateTemplate[V, +E[v] <: Templated[v, E]] extends Template[V, E] {
		self :E[V] with IntermediateTemplate[V, E] =>

		class IntermediateInner[R[+e <: Trait[U]] <: Trait[U], U]
		                       (convert :Convert[V, U] { type Res[+e <: Trait[U]] = R[e] })
			extends Inner[R, U](convert)
		{
			override def apply() :R[E[U]] = convert(self)
		}
	}

	trait Intermediate2[V]
		extends Trait[V] with Intermediate2Template[V, Intermediate2[V]] with Template[V, Intermediate2]
	trait Intermediate2Template[V, +Same <: Intermediate2[V]] {
		this :Same with Intermediate2Template[V, Same] =>
	}

	trait Sub[V] extends Intermediate[V] with Intermediate2[V] with SubTemplate[V, Sub, Sub[V]]

	trait SubTemplate[V, +E[v] <: Templated[v, E], +Same <: E[V] with Sub[V]]
		extends IntermediateTemplate[V, E]
		   with Intermediate2Template[V, Same]
	{ self :Same with SubTemplate[V, E, Same] =>

		class SubInner[R[+e <: Trait[U]] <: Trait[U], U]
		              (convert :Convert[V, U] { type Res[+e <: Trait[U]] = R[e] })
			extends IntermediateInner[R, U](convert)
		{
			override def apply() :R[E[U]] = convert(??? :Templated[V, E])
		}
	}

//	trait ~[L, R]
//	class Record[X] {
//		def |~[K, V](entry :(K, V)) :Record[X ~ (K, V)] = ???
//	}
//	def append[X, K, V](record :Record[X], key :K, value :V) :Record[X ~ (K, V)] =
//		record |~ ((key, value))
//	trait Bound
//	trait Base[-A <: Bound]
//	trait ContraVariantTemplate[-A <: Bound, +Cons[X <: Bound] <: Base[X], +Same <: Base[A]] {
//		def cons[B <: Bound] :Cons[B]
//		def same :Same
//	}
//	trait InvariantTemplate[A <: Bound, +Cons[X <: Bound] <: Base[X]]
//		extends ContraVariantTemplate[A, ({ type T[X <: Bound] = Cons[_ >: X <: Bound] })#T, Cons[A]]
//	trait Invariant[A <: Bound] extends Base[A] with InvariantTemplate[_ >: A, Invariant]

//	println(List(1, -1, 0).sortWith((x, y) => x*x <= y*y))

//	type Cons[O]
//	trait Bound
//	trait Variant[C[A] <: Cons[A]]
//	trait Invariant[C[A] <: Cons[A], O <: Bound]
//	type InvariantUnder[-B <: Bound] = Invariant[Cons, O] forSome { type O >: B <: Bound; type Cons[A] }
//
//	def cons[B <: Bound](a :InvariantUnder[B]) :LazyList[InvariantUnder[B]] =
//		a #:: LazyList.empty[InvariantUnder[B]]

//	trait High[T[_]]
//
//	trait Sub[T[-_]] extends High[T]
//
//	type Split[X, Y, Z] = (X => Y, Y => Z)
//	def split[X, Z](f :X => Z) :Opt[(X => Y, Y => Z)] forSome { type Y } = ???
//
//	split((_:Int).toString) match {
//		case split :Opt[Split[Int, a, String]] if split.isDefined =>
//			val (_1, _2) = split.get
//			_1 andThen _2
//			split.get._1 andThen split.get._2
//	}
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
