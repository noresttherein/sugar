package net.turambar.slang.typist


/*
object unions {
	import UnionType._

	final class \/[A, B] private (private val value :Any, pos :(A || B)#any[_]) {

		def to[T :UnionType[A \/ B]#any] :Option[T] =
			if (implicitly[T in (A \/ B)] == pos) Some(value.asInstanceOf[T])
			else None

//		def as[T] = new MemberCase[T, A, B](this)

		def apply[M :UnionType[A \/ B]#any, O](f :M=>O) :Option[O] = to[M].map(f)
	}

	type ||[A, B] = UnionType[A \/ B]


	class UnionType[U] private(private val value :Any) extends AnyVal {
		type *[M] = M in U
		type any[M] = M in U

		override def toString = value.toString

		def apply[O](f :PartialFunction[Any, O]) :O =
			f.applyOrElse(value, throw new MatchError(s"Not covered union type member $value"))

		def pick[O](f :PartialFunction[Any, O]) :Option[O] = f.lift(value)
	}


	object UnionType {
		implicit def box[T, U](value: T)(implicit member: T in U): UnionType[U] = new UnionType(value)

		object Case {
			def unapply[U](union :UnionType[U]) :Option[Any] = Some(union.value)
		}

		class in[M, U] private(private[unions] val pos :Int) extends AnyVal {
			@inline private[unions] final def or[A] :M in (U \/ A) = new in(pos+1)
			override def toString = s"UnionType.in($pos)"
		}

		sealed class MemberOfTheRest {
			implicit def memberOfTheRest[M, A, U](implicit rest :M in U) :M in (U \/ A) = rest.or[A]
		}

		object in extends MemberOfTheRest {
			//		@inline private def apply[M, U] = make[M, U]
			implicit def self[T] :T in T = new in[T, T](0)
			implicit def firstOf[A, B] :B in (A \/ B) = new (B in (A \/ B))(0)

			private final val first = new in[Any, Any](0)
		}

//		class MemberCase[M, +A, +B](private val union :A \/ B) extends AnyVal {
//			@inline def apply[O, U >: A \/ B](f :M=>O)(implicit member : M in U) :Option[O] = union.to[M].map(f)
//		}

	}


}
*/
