package net.turambar.slang.funny

import scala.annotation.unspecialized
import Fun.specializations._

import scala.reflect.{ClassTag, classTag}


object Fun {

	/** Declaration of type groups usable as arguments to scala `@specialized` annotation. */
	object specializations {
		/** Types for which `Function1`'s result type is specialized. */
		final val Fun1Results = new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)
		/** Types for which `Function1`'s argument type is specialized. */
		final val Fun1Args = new Specializable.Group(Int, Long, Float, Double)
		/** Types for which `Function2`'s argument types are specialized. */
		final val Fun2Args = new Specializable.Group(scala.Int, scala.Long, scala.Double)
		/** Type specialization of the first argument of `Function2`. */
		final val Fun2Arg1 = Fun2Args
		/** Type specialization of the second argument of `Function2`. */
		final val Fun2Arg2 = Fun2Args
		/** Types for which `Function2`'s result type is specialized. */
		final val Fun2Results = new Specializable.Group(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double)
	}

	private def nameOf(f :Nothing=>Any, default :String) :String = f.toString match {
		case "<function1>" => default
		case s => s
	}

	/** Creates a new named function. The string given as the argument to the first application is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function1&gt;'. Note that this results in wrapping
	  * the function given as the second argument and thus introduces overhead of an additional virtual method call.
	  * For this reason this method is most useful during development, as it can be very helpful with debugging.
	  * @param name name to use as the textual repesentation of the given function
	  * @return a factory of function wrappers, with overloaded `apply` method accepting single- and two-argument functions.
	  */
	@inline def apply(name :String) :FunctionNamer = new FunctionNamer(name)


	/** Creates a new named function. The string given as the argument to the first application is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function1&gt;'. Note that this results in wrapping
	  * the function given as the second argument and thus introduces overhead of an additional virtual method call.
	  * For this reason this method is most useful during development, as it can be very helpful with debugging.
	  * @param name name to use as the textual repesentation of the given function
	  * @return a factory of function wrappers, with overloaded `apply` method accepting single- and two-argument functions.
	  */
	@inline def fun(name :String) :FunctionNamer = new FunctionNamer(name)


	/** Creates a composition of two functions. If any of the functions is a [[ComposableFun]] instance, its
	  * `andThen` or `compose` method is used as appropriate. In the other case, a manually created instance
	  * with a more helpful `toString` method is returned. Additionally, [[ComposableFun]]s can coalesce the result
	  * in some special cases such as identity functions, constant functions or functions throwing exceptions. This can
	  * yield performance benefits in situations where such functions are commonly used in longer composition chains.
	  * @param outer function to apply to the result of `inner`
	  * @param inner first function to apply
	  * @return a [[ComposableFun]] instance.
	  */
	@inline def compose[@specialized(Fun1Args)A, @specialized(Fun1Results) B, @specialized(Fun1Results) C]
	                   (outer: B => C, inner: A => B): A => C =
		(outer, inner) match {
			case (fc: ComposableFun[B, C], _) => outer compose inner
			case (_, gc :ComposableFun[A, B]) => gc andThen outer
			case _ => new ComposedFun(inner, outer)
		}

	/** Creates a composition of two functions using the given string as its textual representation.
	  * If any of the functions is a [[ComposableFun]] instance, its `andThen` or `compose` method is used as appropriate.
	  * In the other case, a manually created instance with a more helpful `toString` method is returned.
	  * Additionally, [[ComposableFun]]s can coalesce the result in some special cases such as identity functions,
	  * constant functions or functions throwing exceptions. This can yield performance benefits in situations
	  * where such functions are commonly used in longer composition chains.
	  * @param name text to use in `toString` implementation.
	  * @param outer function to apply to the result of `inner`
	  * @param inner first function to apply
	  * @return a [[ComposableFun]] instance.
	  */
	@inline def compose[@specialized(Fun1Args)A, @specialized(Fun1Results) B, @specialized(Fun1Results) C]
	                   (name :String)(outer: B => C, inner: A => B): A => C =
		new ComposedFun[A, B, C](inner, outer) {
			override val toString = name
		}


	/** Creates a composition of two functions using the given names in its textual representation.
	  * If any of the functions is a [[ComposableFun]] instance, its `andThen` or `compose` method is used as appropriate.
	  * In the other case, a manually created instance with a more helpful `toString` method is returned.
	  * Additionally, [[ComposableFun]]s can coalesce the result in some special cases such as identity functions,
	  * constant functions or functions throwing exceptions. This can yield performance benefits in situations
	  * where such functions are commonly used in longer composition chains.
	  * @param outerName name of the outer function to use in `toString` implementation.
	  * @param innerName name of the inner function to use in `toString` implementation.
	  * @param outer function to apply to the result of `inner`
	  * @param inner first function to apply
	  * @return a [[ComposableFun]] instance.
	  */
	@inline def compose[@specialized(Fun1Args) A, @specialized(Fun1Results) B, @specialized(Fun1Results) C]
	                   (outerName :String, innerName :String)(outer :B=>C, inner :A=>B) :A=>C =
		compose(ComposedFun.name(outerName, innerName))(outer, inner)


	/** Equivalent to `scala.identity[X]`, but is specialized and overrides `compose` (and `andThen`)
	  * for reduced overhead of function composition. Additionally, it provides a more informative `toString` output.
	  */
	@inline def idfun[@specialized(Fun1Args) X]: X => X = new IdFun[X]

	/** A function doing nothing and returning no value. */
	final val noop = constfun[Any, Unit](())

	/** A constant function equivalent to `(_:X) => value`, but with a more helpful `toString` and performing reduction on composition.
	  * For any `g :Y=>O`, `f endThen g` eagerly evaluates to `constfun[X, O](g(value))`,
	  * while for any `h :A=>X`, `f compose h` evaluates simply to `constfun[A, Y](value)`. Note that if in the latter case
	  * inner function `h` would in any circumstances throw an exception, it would be silently suppressed with the
	  * constant result defined here taking precedence, unless the function `h` itself was defined by any of the `thenThrow`
	  * methods declared here or is the result of composing such a function.
	  */
	@inline final def constfun[@specialized(Fun1Args) X, @specialized(Fun1Results) Y](value :Y) :X => Y = new ConstFun(value)


	/** A function throwing the exception returned for its argument by the passed constructor function.
	  * Provides an informative textual representation and performs reduction during composition, ignoring any
	  * function passed to its `andThen`, returning itself instead.
	  */
	@inline final def thenThrow[X, E <: Exception :ClassTag](exception :X => E) :X=>Nothing = new ThrowFun(exception)

	/** A function which always throws the given exception. Provides informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method.
	  */
	@inline final def thenThrow(exception :Exception) :Any=>Nothing = thenThrow(constfun[Any, Exception](exception))(ClassTag(exception.getClass))

	object errors {
//		@inline final def unsupported[X, E<:Exception]
	}



	trait ComposableFun[@specialized(Fun1Args) -X, @specialized(Fun1Results) +Y] extends (X=>Y) {
		override def compose[A](g: A => X): A => Y = g match {
			case _ :GenericIdentity[_] | _ :UpCast[_, _] | _ :ConstFun[_, _] | _ :ThrowFun[_, _] =>
				g andThen this
			case _ => new ComposedFun(g, this)
		}

		override def andThen[A](g: Y => A): X => A = g match {
			case _ :GenericIdentity[_] | _ :UpCast[_, _] | _ :ConstFun[_, _] | _ :ThrowFun[_, _] =>
				g compose this
			case _ => new ComposedFun(this, g)
		}
	}


	abstract class #=>:[@specialized(Fun1Args) -X, @specialized(Fun1Results) +Y](override val toString :String) extends ComposableFun[X, Y]

	final class NamedFun1[@specialized(Fun1Args) -X, @specialized(Fun1Results) +Y](name :String, f :X=>Y) extends ComposableFun[X, Y] {
		override def apply(v1: X): Y = f(v1)
		override def toString :String = name
	}



	trait GenericIdentity[@specialized(Fun1Args) X] extends (X => X) {
		final override def apply(v1: X): X = v1

		@unspecialized
		override def compose[A](g: A => X): A => X = g

		@unspecialized
		override def andThen[A](g: X => A): X => A = g

		override def toString = "{x=>x}" //todo print specialization
	}

	final class IdFun[@specialized(Fun1Args) X] extends GenericIdentity[X] {

		@unspecialized
		override def compose[A](g: A => X): g.type = g

		@unspecialized
		override def andThen[A](g: X => A): g.type = g
	}

	object IdFun {
		@inline implicit def apply[@specialized(Fun1Args) X] :X=>X = new IdFun[X]

		@inline def unapply[X, Y](f :X=>Y) :Boolean = f.isInstanceOf[GenericIdentity[_]]

	}


	object ConstFun {
		def unapply[Y](f :Nothing=>Y) :Option[Y] = f match {
			case const :ConstFun[_, Y] => Some(const.value)
			case _ => None
		}
	}


	private final class ConstFun[@specialized(Fun1Args) -X, @specialized(Fun1Results) +Y](val value :Y) extends (X => Y) {
		override def apply(v1: X): Y = value

		@scala.annotation.unspecialized
		override def compose[A](g: A => X): A => Y = g match {
			case _ :ThrowFun[_, _] => g andThen this
			case _ => new ConstFun(value) //new instance to drop specialization on the argument
		}

		override def andThen[A](g: Y => A): X => A = g match {
			case _ :ThrowFun[_, _] => g compose this
			case _ => try {
				new ConstFun(g(value))
			} catch {
				case e :Exception => thenThrow(e)
			}
		}

		override def equals(that :Any) :Boolean = that match {
			case const :ConstFun[_, _] => (this eq const) || value==const.value
			case _ => false
		}

		override def hashCode :Int = value.hashCode

		override def toString = s"{_ => $value}"
	}



	private final class ThrowFun[@specialized(Fun1Args) -X, E <: Exception :ClassTag](exception :X => E) extends ComposableFun[X, Nothing] {

		override def apply(v1: X): Nothing = throw exception(v1)

		override def compose[A](g: A => X): A => Nothing = g match {
			case _ :GenericIdentity[_] | _ :UpCast[_, _]  => g andThen this
			case _ :ComposableFun[_, _] => thenThrow(g andThen exception)
			case _ => thenThrow(exception compose g)
		}

		override def andThen[A](g: Nothing => A): X => A = this

		override def toString :String = "throw "+classTag[E].runtimeClass.getName
	}


	private class ComposedFun[@specialized(Fun1Args) -X, @specialized(Fun1Args) Y, @specialized(Fun1Results) +Z](val inner :X=>Y, val outer :Y=>Z)
		extends ComposableFun[X, Z]
	{
		override def apply(x: X): Z = outer(inner(x))

		override def equals(that :Any) :Boolean = that match {
			case c :ComposedFun[_, _, _] => (c eq this) || c.inner==inner && c.outer==outer
			case _ => false
		}

		override def hashCode :Int = inner.hashCode * 31 + outer.hashCode

		override def toString :String = ComposedFun.name(nameOf(outer, "f"), nameOf(inner, "g")) //s"<${nameOf(outer, "f")} * ${nameOf(inner, "g")}>"

		private def nameOf(f :Nothing=>Any, default :String) = f.toString match {
			case "<function1>" => default
			case "<f * g>" => "<...>"
			case partial if partial.startsWith("<f *") || partial.endsWith("* g>") => partial
			case full if f.isInstanceOf[ComposedFun[_, _, _]] => full.substring(1, full.length-1)
			case s => s
		}
	}

	object ComposedFun {
		def name(outer :String, inner :String) :String = s"<$outer * $inner>"
	}




	/** Constructor and extractor of [[CastFun]] instances representing a safe cast from a type `X` to its supertype `Y`.
	  * An implicit instance of `CastFun[X, Y]` is present whenever there is an implicit value for `X &lt;:&lt; Y`.
	  * However, `CastFun` instances can be used to obtain transitive proofs about the super-type relation for
	  * complex types.
	  */
	object CastFun {

		/** If `f` is an identity function (an instance of [[net.turambar.slang.funny.Fun.IdFun]]),
		  * returns a `CastFun[X, Y]` evidence of the fact, that instances of `X` can be used anywhere
		  * where an instance of `Y` is required, as well as transitive relation for type constructors
		  * parameterized with `X` and `Y`.
		  */
		def unapply[X, Y](f :X=>Y) :Option[CastFun[X, Y]] = f match {
			case cast :CastFun[X, Y] => Some(cast)
			case id :GenericIdentity[_] => Some(cast.asInstanceOf[CastFun[X, Y]])
//			case _ :IdFun[_] | _ :CastFun[_, _] => casts.asInstanceOf[Option[(CastFun[X, Y], CastFun[Y, X])]]
			case _ => None
		}


		def apply[X<:Y, Y] :UpCast[X, Y] = upcast[X]

		def apply[X, Y](evidence :X <:< Y) :CastFun[X, Y] = cast.asInstanceOf[CastFun[X, Y]]

		implicit def upcast[X] :UpCast[X, X] = cast.asInstanceOf[UpCast[X, X]]



		private[this] final val cast = new IdentityCast[Any]


	}

	/** A proof that `X &lt;: Y`, similar to `X &lt;:&lt; Y`, but providing transitive proofs for complex types
	  * parameterized with `X` and/or `Y`. When treated as a function, it is a true identity relationship:
	  * not only `apply` returns the argument itself, but also composition functions return their arguments
	  * instead of complex objects (as identity is the neutral element for function composition).
	  * @tparam X subtype of `Y`
	  * @tparam Y supertype of `X`
	  */
	sealed trait CastFun[-X, +Y] extends (X=>Y) {
		/** Identity function, an instance of [[net.turambar.slang.funny.Fun.IdFun]]. */
		def ident :X=>Y

		implicit val <:< : X<:<Y

		implicit def cocast[T[+A]] :CastFun[T[X], T[Y]]
		implicit def contracast[T[-A]] :CastFun[T[Y], T[X]]

		implicit def covariant[T[+A]] :T[X] <:< T[Y]
		implicit def contravariant[T[-A]] :T[Y] <:< T[X]
		implicit def pair[T[-A, +B]] : T[Y, X] <:< T[X, Y]


	}




	class UpCast[-X<:Y, +Y] extends CastFun[X, Y] {
		def apply(x :X) :Y = x

		@scala.annotation.unspecialized
		override def compose[A](g: A => X): A => Y = g

		@scala.annotation.unspecialized
		override def andThen[A](g: Y => A): X => A = g

		def ident :X => Y = idfun[X]

		final implicit val <:< :X<:<Y = implicitly[X<:<X]

		final implicit override def cocast[T[+A]] :UpCast[T[X], T[Y]] = CastFun[T[X], T[Y]]

		final implicit override def contracast[T[-A]] :UpCast[T[Y], T[X]] = CastFun[T[Y], T[X]]

		final implicit override def covariant[T[+A]] :T[X]<:<T[Y] = implicitly[T[X] <:< T[X]]

		final implicit override def contravariant[T[-A]] :T[Y]<:<T[X] = implicitly[T[Y] <:< T[Y]]

		final implicit override def pair[T[-A, +B]] :T[Y, X] <:< T[X, Y] = implicitly[T[Y, X] <:< T[Y, X]]

		override def toString = "((_:X) :Y)"
	}

	private final class IdentityCast[X] extends UpCast[X, X] with GenericIdentity[X] {
		override def ident :X=>X = this
	}








//	@inline final def __=>[T](value :T) = { _ :Any => value }

	class FunctionNamer(override val toString :String) extends AnyVal { name =>

		def apply[@specialized(Fun1Args) T1, @specialized(Fun1Results) R](f :T1=>R) :T1 => R = new NamedFun1(toString, f)

		def apply[@specialized(Fun2Arg1) T1, @specialized(Fun2Arg2) T2, @specialized(Fun2Results) R](f :(T1, T2)=>R) :(T1, T2) => R =
			new Function2[T1, T2, R]
			{
				override def apply(v1: T1, v2: T2): R = f(v1, v2)

				@unspecialized
				override def curried: T1 => T2 => R = Fun(toString)(super.curried)

				@unspecialized
				override def tupled: ((T1, T2)) => R = Fun(s"($toString)")(super.tupled)

				override val toString = name.toString
			}
	}


}
