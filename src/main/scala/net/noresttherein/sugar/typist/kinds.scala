package net.noresttherein.sugar.typist




@SerialVersionUID(Ver)
object kinds {
//	type Subtype[U] = { type T[X <: U] = X }
	type Any1[+_]        = scala.Any
	type AnyRef1[+_]     = scala.AnyRef
	type Any2[+_, +_]    = scala.Any
	type AnyRef2[+_, +_] = scala.AnyRef
	type Nothing1[-_]    = scala.Nothing
	type Identity[+X]    = X
	type Self[+X]        = X
	type Fixed[Y]        = { type T[X] = Y }

	type =>:[-X[A], +Y[A]] = GenericFun[X, Y] //right associative

	/** Groups type aliases for applying `A` a number of times (`A[A[X]]`, `A[A[A[X]]]`, etc.). */
	type Pow[A[X]] = {
		type _2[X] = A[A[X]]
		type _3[X] = A[A[A[X]]]
		type _4[X] = A[A[A[A[X]]]]
		type _5[X] = A[A[A[A[A[X]]]]]
		type _6[X] = A[A[A[A[A[A[X]]]]]]
		type _7[X] = A[A[A[A[A[A[A[X]]]]]]]
		type _8[X] = A[A[A[A[A[A[A[A[X]]]]]]]]
		type _9[X] = A[A[A[A[A[A[A[A[A[X]]]]]]]]]
	}

	trait GenericFun[-X[A], +Y[A]] extends Serializable { outer =>
		def apply[T](x :X[T]) :Y[T]

		def existential: X[_] => Y[_] = apply(_)

		def andThen[Z[_]](f :GenericFun[Y, Z]) :GenericFun[X, Z] = new GenericFun[X, Z] {
			override def apply[T](x :X[T]) :Z[T] = f(outer(x))
		}

		def compose[W[_]](f :GenericFun[W, X]) :GenericFun[W, Y] = f andThen this
	}

	trait BoxFun[+Y[A]] extends GenericFun[Identity, Y] {
		override def apply[T](x :T) :Y[T]
	}

	trait UnboxFun[-X[A]] extends GenericFun[X, Identity] {
		override def apply[T](x :X[T]) :T
	}



	def ident[X[A]] :GenericFun[X, X] = identity.asInstanceOf[GenericFun[X, X]]

	private[this] final val identity = new GenericFun[Identity, Identity] {
		override def apply[T](x :T) = x
	}

}
