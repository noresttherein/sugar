import scala.annotation.tailrec
import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
	trait TypeClass[T]
	class Hehe[T :TypeClass](private val cls :Class[T], val tpe :Int)
	{
		def this(tpe :Int) = //(implicit tag :ClassTag[T]) =
			this(null, tpe)
	}

	class C[@specialized T, @specialized S]
	println(new C[Byte, Int].getClass.getName)
	println(new C[Short, Int].getClass.getName)
	println(new C[Int, Int].getClass.getName)
	println(new C[Long, Int].getClass.getName)
	println(new C[Char, Int].getClass.getName)
	println(new C[Float, Int].getClass.getName)
	println(new C[Double, Int].getClass.getName)
	println(new C[Boolean, Int].getClass.getName)
	println(new C[Unit, Int].getClass.getName)
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
//		implicit def covariant[T :Covariant] :Covariant[Option[T]] = new Covariant
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
