import net.noresttherein.slang.typist.InferTypeParams.Conforms


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
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


	trait Base[S] {
		type Subject = S
	}

	trait Extended {
		class Subject
	}
}