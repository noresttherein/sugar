import scala.annotation.tailrec
import scala.collection.Factory
import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
//	println(scala.reflect.runtime.universe.reify {
//	}.tree)

	//	val map = Map[Int, Int]()
//	map.getOrElse(1, 1)
//	     else if (mark == Pinkie) "Yayayaayoo"
//	          else "?"

//	println(scala.reflect.runtime.universe.reify {
//		def t[T] = fff__[T]
//	}.tree)
//
//	trait WorseCovariants
//	trait WorseInvariants extends WorseCovariants
//	trait Covariants extends WorseInvariants
//	trait Invariants extends Covariants
//	trait Implicits extends Invariants
//	trait WorseCovariant[+T] extends Implicits
//	trait WorseInvariant[T] extends WorseCovariant[T]
//
//	class Covariant[+T] extends WorseCovariant[T]
//
//	class Invariant[T] extends Covariant[T] with WorseInvariant[T]
//
//
//	object Implicits {
//		implicit val int = new Invariant[Int]
//		implicit val short = new Invariant[Short]
//	}
//
//	object WorseCovariants {
//		implicit def covariant[T :WorseCovariant] :WorseCovariant[Option[T]] = new Covariant
//	}
//
//	object WorseInvariants {
//		implicit def invariant[T :WorseInvariant] :WorseInvariant[Option[T]] = new Invariant
//	}
//
//	object Covariants {
//		implicit def covariant[T :Covariant] :Covariant[Option[T = new Covariant
//	}
//
//	object Invariants {
//		implicit def invariant[T :Invariant] :Invariant[Option[T]] = new Invariant
//	}
//
//
//
//	println(scala.reflect.runtime.universe.reify{
//		implicitly[WorseInvariant[Option[Int]]]
//	}.tree)
}
