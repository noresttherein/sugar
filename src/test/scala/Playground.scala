import net.noresttherein.slang.typist.InferTypeParams.Conforms


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {

	trait Subject {
		type E
	}

	abstract class Projection[S <: Subject] {
		type Project[X] <: Subject { type E = X }
	}

	implicit class ProjectSubject[S <: Subject](private val self :S) extends AnyVal {
		def project[X](implicit p :Projection[S]) :p.Project[X] = p.asInstanceOf[p.Project[X]]
	}
//	implicit class ProjectSubject[S <: Subject](private val self :S) extends AnyVal {
//		def project[X](implicit p :Projection[_ >: S <: Subject]) :p.Project[X] = ???
//	}
//	abstract class ProjectionAvailable[-S <: T, T <: Subject] //extends (S => T)
//	implicit def ProjectionAvailable[S <: Subject](implicit p :Projection[S]) :ProjectionAvailable[S, S] = ??? //(s :S) => s
//
//	implicit def ProjectionSubject[S <: T, T <: Subject](s :S)(implicit witness :ProjectionAvailable[S, T]) =
//		new ProjectionSubject[T](s)
//
//	class ProjectionSubject[S <: Subject](private val self :S) extends AnyVal {
//		def project[X](implicit p :Projection[S]) :p.Project[X] = p.asInstanceOf[p.Project[X]]
//	}

	class Box[X] extends Subject { type E = X }

	object Box {
		implicit def projection[A] :Projection[Box[A]] { type Project[X] = Box[X] } = ???
	}

	class Adapter[S <: Subject] extends Subject { type E = S#E }

	object Adapter {
		implicit def adapterProjection[S <: Subject](implicit p :Projection[S])
			:Projection[Adapter[S]] { type Project[X] = Adapter[p.Project[X]] } = ???
	}

	val res = new Adapter[Box["E"]].project["F"]

//	class Specific extends Adapter[Box["E"]]
//	val spec = (new Specific).project["F"]
//	spec :Nothing


//	implicit def format(s :String, i :Int) :String = s + i

//	implicitly[(String, Int) => String]

//	trait F[X]
	trait Base {
		type X
	}

	type F[Y] = Base { type X = Y }

	trait FF[X, Y] //extends F[X]

//	type F[X] = FF[X, _]

	trait FFF[A, B] extends Base { type X = A }//FF[X, X]

	class High[A[B] <: F[B]]

	trait Sig {
		def test :High[A] forSome { type A[B] <: F[B] }
	}

	class Struct[T[A] <: FFF[A, B], B] extends Sig {
		override def test :High[T] = ???
	}


	trait Clause {
		type T

		def set(t :T)
		def get :T
	}

	type SubclauseOf[-C <: Clause] = Clause { type T >: C <: Clause }

	def test[C <: Clause, S <: SubclauseOf[C]](s :S) = {
		s.set(s.get)
	}


	trait FromClause

	class Left extends FromClause

	trait Join[+F <: FromClause, T[O] <: MappingFrom[O]]

	trait Mapping {
		type Origin
		type Subject
	}
	type MappingFrom[O] = Mapping { type Origin = O }

	trait GenericMapping[S, O] extends Mapping {
		type Origin = O
		type Subject = S
	}
	type GenericMappingFrom[O] = GenericMapping[_, O]
	type GenericSubject[S] = { type M[O] = GenericMapping[S, O] }

	trait RowSource[M[O] <: MappingFrom[O]]

	def join[F <: FromClause, M[O] <: MappingFrom[O], R[O] <: GenericMapping[S, O], S]
	        (from :F, source :RowSource[M])
	        (implicit inferR :Conforms[M[Any], R[Any], GenericMapping[S, Any]])
			:F Join R = ???


	def test[M[O] <: MappingFrom[O], R[O] <: GenericMapping[S, O], S]
	        (source :RowSource[M])
	        (implicit inferR :Conforms[M[Any], R[Any], GenericMapping[S, Any]])
			:FromClause Join M =
		test(source)

	class Concrete[O] extends GenericMapping["Subject", O]
	def source :RowSource[Concrete] = ???


//	class InferSubject[R[O] <: MappingFrom[O], T[O] <: U[O], +U[O] <: GenericMapping[_, O]]
//
//	implicit def InferSubject[R[O] <: GenericMapping[_, O]] = new InferSubject[R, R, R]
//
//	def inferSubject[L <: FromClause, R[O] <: MappingFrom[O], T[O] <: GenericMapping[S, O], S]
//	                (left :L, source :RowSource[R])(implicit infer :InferSubject[R, T, GenericSubject[S]#M])
//			:L Join R = ???

//	val join = inferSubject(new Left, source)
//	join :Int


	sealed abstract class JoinedRelationSubject[J[+F <: FromClause, M[O] <: MappingFrom[O]] <: F Join M,
		R[O] <: MappingFrom[O], T[O] <: U[O], +U[O] <: GenericMapping[_, O]]
	{
		def apply(rows :RowSource[R]) :RowSource[T]

		def apply[F <: FromClause](join :F J T) :F J R
	}

	object JoinedRelationSubject {
		private[this] val instance =
			new JoinedRelationSubject[Join, GenericMappingFrom, GenericMappingFrom, GenericMappingFrom] {
				override def apply(rows :RowSource[GenericMappingFrom]) = rows

				override def apply[F <: FromClause](join :Join[F, GenericMappingFrom]) = join
			}

		implicit def identity[J[+F <: FromClause, M[O] <: MappingFrom[O]] <: F Join M, R[O] <: GenericMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]

		type InferSubject[J[+F <: FromClause, M[O] <: MappingFrom[O]] <: F Join M,
		                  R[O] <: MappingFrom[O], T[O] <: GenericMapping[S, O], S] =
			JoinedRelationSubject[J, R, T, GenericSubject[S]#M]

	}
	import JoinedRelationSubject.InferSubject

	def newJoin[L <: FromClause, R[O] <: MappingFrom[O], T[O] <: GenericMapping[S, O], S]
	           (left :L, right :RowSource[R])(implicit inferer :InferSubject[Join, R, T, S]) :L Join R = ???

	val join = newJoin(new Left, source)//(JoinedRelationSubject.identity[Join, Concrete])
//	join :Int
//	val r = test(source)
//	r :(Concrete["Org"], GenericMapping["Subject", "Org"])

//	class AndImpl[U, X](val left :Bound[U], val next :X) extends And[Bound[U]#Self, X]


//	trait Base[S] {
//		type Subject = S
//	}
//
//	trait Extended {
//		class Subject
//	}
}