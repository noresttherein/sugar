package net.noresttherein.sugar.funny




@SerialVersionUID(Ver)
object generic {
//	type Subtype[U] = { type T[X <: U] = X }
	type Any[+_]     = scala.Any
	type Nothing[-_] = scala.Nothing
	type Ident[+X]   = X //consider: renaming to Identity
	type Fixed[Y]    = { type T[X] = Y }

	type =>:[-X[A], +Y[A]] = GenericFun[X, Y]

	trait GenericFun[-X[A], +Y[A]] extends Serializable { outer =>
		def apply[T](x :X[T]) :Y[T]

		def existential: X[_] => Y[_] = apply(_)

		def andThen[Z[_]](f :GenericFun[Y, Z]) :GenericFun[X, Z] = new GenericFun[X, Z] {
			override def apply[T](x :X[T]) :Z[T] = f(outer(x))
		}

		def compose[W[_]](f :GenericFun[W, X]) :GenericFun[W, Y] = f andThen this

	}

	trait BoxFun[+Y[A]] extends GenericFun[Ident, Y] {
		override def apply[T](x :T) :Y[T]
	}

	trait UnboxFun[-X[A]] extends GenericFun[X, Ident] {
		override def apply[T](x :X[T]) :T
	}



	def ident[X[A]] :GenericFun[X, X] = identity.asInstanceOf[GenericFun[X, X]]

	private[this] final val identity = new GenericFun[Ident, Ident] {
		override def apply[T](x :T) = x
	}

}
