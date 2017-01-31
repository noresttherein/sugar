package net.turambar.slang

/**
  * @author Marcin Mościcki
  */
package object typist {


	final class UpperBound[X, Y, U] private[typist] (val _1 :X<:<U, val _2 :Y<:<U) {
		type T = U
		@inline def left(x :X) :U = x.asInstanceOf[U]
		@inline def right(y :Y) :U = y.asInstanceOf[U]
	}


	sealed abstract class ProperUpperBound private[typist] {
		@inline implicit final def properUpperBound[X<:U, Y<:U, U] :UpperBound[X, Y, U] = instance.asInstanceOf[UpperBound[X, Y, U]]

		final protected[this] val instance = new UpperBound[Any, Any, Any](implicitly[Any<:<Any], implicitly[Any<:<Any])
	}

	sealed abstract class SelfUpperBound private[typist] extends ProperUpperBound {
		@inline implicit final def leftUpperBound[X, Y<:X] :UpperBound[X, Y, X] = instance.asInstanceOf[UpperBound[X, Y, X]]
		@inline implicit final def rightUpperBound[X<:Y, Y] :UpperBound[X, Y, Y] = instance.asInstanceOf[UpperBound[X, Y, Y]]

		//		@inline implicit final def leftUpperBound[X, Y](implicit ev :Y<:<X) :UpperBound[X, Y, X] = instance.asInstanceOf[UpperBound[X, Y, X]]
		//		@inline implicit final def rightUpperBound[X, Y](implicit ev :X<:<Y) :UpperBound[X, Y, Y] = instance.asInstanceOf[UpperBound[X, Y, Y]]

		//		private[this] val instance = new UpperBound[Any, Any, Any](implicitly[Any<:<Any], implicitly[Any<:<Any])
	}

	object UpperBound extends SelfUpperBound {
		final class Binder[X, Y] private[UpperBound] {
			@inline def apply[U]()(implicit u :UpperBound[X, Y, U]) :UpperBound[X, Y, U] = u
		}

		@inline implicit def apply[X, Y] :Binder[X, Y] = new Binder[X, Y]

		@inline implicit final def identityUpperBound[X] :UpperBound[X, X, X] = leftUpperBound[X, X].asInstanceOf[UpperBound[X, X, X]]
		//		@inline implicit final def identityUpperBound[X, Y](implicit ev :X=:=Y) :UpperBound[X, Y, X] = leftUpperBound[X, X].asInstanceOf[UpperBound[X, Y, X]]
	}

	//	trait A {
	//		def iAmA = ???
	//	}
	//	class B extends A
	//	class C extends A
	//
	//	def ubind[A, B, C](l :A, r :B)(implicit u :UpperBound[A, B, C]) :C = u.left(l)
	//	def lbind[A, B, C](l :A, r :B)(implicit u :LowerBound[A, B, C]) :C = ???
	//
	//	val u = ubind(new B, new C)
	//	u.iAmA
	//
	//	val lub = UpperBound[B, C]()
	//	val a :lub.T = ???
	//	a.iAmA

	final class LowerBound[X, Y, L] private[typist] (val _1 :L<:<X, val _2 :L<:<Y) {
		type T = L
		@inline def left(l :L) :X = l.asInstanceOf[X]
		@inline def right(l :L) :Y = l.asInstanceOf[Y]
	}


	sealed abstract class ProperLowerBound private[typist] {
		@inline implicit def properLowerBound[X>:L, Y>:L, L] :LowerBound[X, Y, L] = instance.asInstanceOf[LowerBound[X, Y, L]]

		final protected[this] val instance = new LowerBound[Any, Any, Any](implicitly[Any<:<Any], implicitly[Any<:<Any])
	}

	sealed abstract class SelfLowerBound private[typist] extends ProperLowerBound {
		@inline implicit final def leftLowerBound[X, Y](implicit ev :X<:<Y) :LowerBound[X, Y, X] = instance.asInstanceOf[LowerBound[X, Y, X]]
		@inline implicit final def rightLowerBound[X, Y](implicit ev :Y<:<X) :LowerBound[X, Y, Y] = instance.asInstanceOf[LowerBound[X, Y, Y]]


	}

	object LowerBound extends SelfLowerBound {
		@inline implicit final def identityLowerBound[X, Y](implicit ev :Y=:=X) :LowerBound[X, Y, X] = instance.asInstanceOf[LowerBound[X, Y, X]]
	}

}
