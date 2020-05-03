package net.noresttherein.slang.typist



/** Verifies that appropriate implicit evidence exists. */
object InferTypeParamsSpec {

	{ //braces to have unqualified type names in the compiler output
		case class Invariant[T](item :T)

		class UpperBound
		class Item extends UpperBound
		//Lower bounds don't work :(
		class LowerBound extends Item

		{ //invariant
			class Super[X <: UpperBound] extends UpperBound
			class Sub[X <: UpperBound] extends Super[X]
			class Concrete extends Sub[Item]

			def infer2[X, O <: Super[T], T <: UpperBound]
			          (x :X)(implicit infer :InferTypeParams[X, O, Super[T]]) :(O, Super[T]) =
				(infer._1(x), infer._2(x))

			val sub = Invariant(infer2(new Sub[Item]))
			//check if Nothing was not inferred
			sub :Invariant[(Sub[Item], Super[Item])]

			val concrete = Invariant(infer2(new Concrete))
			concrete :Invariant[(Concrete, Super[Item])]

		}

		{ //covariant
			class Super[+X <: UpperBound] extends UpperBound
			class Sub[+X <: UpperBound] extends Super[X]
			class Concrete extends Sub[Item]

			def infer2[X, O <: Super[T], T <: UpperBound]
			          (x :X)(implicit infer :InferTypeParams[X, O, Super[T]]) :(O, Super[T]) =
				(infer._1(x), infer._2(x))

			val res = Invariant(infer2(new Sub[Item]))
			res :Invariant[(Sub[Item], Super[Item])]

			val res2 = Invariant(infer2(new Concrete))
			res2 :Invariant[(Concrete, Super[Item])]
		}

		{ //contravariant
			class Super[-X <: UpperBound] extends UpperBound
			class Sub[-X <: UpperBound] extends Super[X]
			class Concrete extends Sub[Item]

			def infer2[X, O <: Super[T], T <: UpperBound]
			          (x :X)(implicit infer :InferTypeParams[X, O, Super[T]]) :(O, Super[T]) =
				(infer._1(x), infer._2(x))

			val res = Invariant(infer2(new Sub[Item]))
			res :Invariant[(Sub[Item], Super[Item])]

			val res2 = Invariant(infer2(new Concrete))
			res2 :Invariant[(Concrete, Super[Item])]
		}

	}

}

