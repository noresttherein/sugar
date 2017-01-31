package net.turambar.slang.funny

import scala.annotation.unspecialized
import Fun.specializations._

import scala.reflect.{ClassTag, classTag}


object Fun {
	
	object specializations {
		final val Fun1ResultTypes = new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)
		final val Fun1ArgTypes = new Specializable.Group(Int, Long, Float, Double)
		final val Fun2Arg1Types = new Specializable.Group(scala.Int, scala.Long, scala.Double)
		final val Fun2Arg2Types = new Specializable.Group(scala.Int, scala.Long, scala.Double)
		final val Fun2ResultTypes = new Specializable.Group(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double)
	}

	def nameOf(f :Nothing=>Any, default :String) :String = f.toString match {
		case "<function1>" => default
		case s => s
	}
	
	@inline def apply(name :String) :FunctionNamer = new FunctionNamer(name)

	@inline def fun(name :String) :FunctionNamer = new FunctionNamer(name)

	@inline def fun[@specialized(Fun1ArgTypes) X, @specialized(Fun1ResultTypes) Y](f :X=>Y) :X=>Y = (v1: X) => f(v1)

	
	
	@inline def compose[@specialized(Fun1ArgTypes)A, @specialized(Fun1ResultTypes) B, @specialized(Fun1ResultTypes) C]
			(outer: B => C, inner: A => B): A => C =
		(outer, inner) match {
			case (fc: ComposableFun[B, C], _) => outer compose inner
			case (_, gc :ComposableFun[A, B]) => gc andThen outer
			case _ => new ComposedFun(inner, outer)
		}
	
	@inline def compose[@specialized(Fun1ArgTypes)A, @specialized(Fun1ResultTypes) B, @specialized(Fun1ResultTypes) C]
			(name :String)(outer: B => C, inner: A => B): A => C =
		new ComposedFun[A, B, C](inner, outer) {
			override val toString = name
		}
	
	@inline def compose[@specialized(Fun1ArgTypes) A, @specialized(Fun1ResultTypes) B, @specialized(Fun1ResultTypes) C]
			(outerName :String, innerName :String)(outer :B=>C, inner :A=>B) :A=>C =
		compose(ComposedFun.name(outerName, innerName))(outer, inner)
	
	
	/** Equivalent to `scala.identity[X]`, but overrides `compose` (and `andThen`) for reduced overhead of function composition. */
	@inline def idfun[@specialized(Fun1ArgTypes) X]: X => X = new IdFun[X]
	
	
	final val noop = constfun[Any, Unit](())
	
	@inline final def constfun[@specialized(Fun1ArgTypes) X, @specialized(Fun1ResultTypes) Y](value :Y) :X => Y = new ConstFun(value)



	@inline final def errorfun[X, E <:Exception :ClassTag](exception :X => E) :X=>Nothing = new ComposableFun[X, Nothing] {

		override def apply(v1: X): Nothing = throw exception(v1)

		override def compose[A](g: (A) => X): (A) => Nothing = errorfun(exception compose g)

		override def andThen[A](g: (Nothing) => A): (X) => A = this

		override def toString = "throw "+classTag[E].runtimeClass.getName
	}

	@inline final def error(exception :Exception) :Any=>Nothing = errorfun(constfun[Any, Exception](exception))(ClassTag(exception.getClass))

	object errors {
//		@inline final def unsupported[X, E<:Exception]
	}
	
	
	
	trait ComposableFun[@specialized(Fun1ArgTypes) -X, @specialized(Fun1ResultTypes) +Y] extends (X=>Y) {
		override def compose[A](g: (A) => X): (A) => Y = new ComposedFun(g, this)
		
		override def andThen[A](g: (Y) => A): (X) => A = new ComposedFun(this, g)
	}
	
	final class NamedFun1[@specialized(Fun1ArgTypes) -X, @specialized(Fun1ResultTypes) +Y](name :String, f :X=>Y) extends ComposableFun[X, Y] {
		override def apply(v1: X): Y = f(v1)
		override def toString = name
	}
	
	
	
	trait GenericIdentity[@specialized(Fun1ArgTypes) X] extends (X => X) {
		final override def apply(v1: X): X = v1
		
		@scala.annotation.unspecialized
		override def compose[A](g: (A) => X): (A) => X = g
		
		@scala.annotation.unspecialized
		override def andThen[A](g: (X) => A): (X) => A = g
		
		override def toString = "{x=>x}"
	}
	
	final class IdFun[@specialized(Fun1ArgTypes) X] extends GenericIdentity[X] {
//		override def apply(x: X): X = x
		
		@scala.annotation.unspecialized
		override def compose[A](g: (A) => X): g.type = g
		
		@scala.annotation.unspecialized
		override def andThen[A](g: (X) => A): g.type = g
	}
	
	object IdFun {
		@inline implicit def apply[@specialized(Fun1ArgTypes) X] :X=>X = new IdFun[X]

		@inline def unapply[X, Y](f :X=>Y) :Boolean = f.isInstanceOf[GenericIdentity[_]]

		
/*
		object inverse {
			@inline def unapply[X, Y](f :X=>Y) :Option[(CastFun[X, Y], CastFun[Y, X])] = f match {
				case f :GenericIdentity[_] => casts
				case _ => None
			}
			
			private[this] val casts = Some(CastFun[Any], CastFun[Any])
		}
*/
		

		
	}
	

	object ConstFun {
		def unapply[Y](f :Nothing=>Y) :Option[Y] = f match {
			case const :ConstFun[_, Y] => Some(const.value)
			case _ => None
		}
	}
	
	
	final class ConstFun[@specialized(Fun1ArgTypes) -X, @specialized(Fun1ResultTypes) +Y](val value :Y) extends (X => Y) {
		override def apply(v1: X): Y = value
		
		@scala.annotation.unspecialized
		override def compose[A](g: (A) => X): A => Y = new ConstFun(value)
		
		override def andThen[A](g: (Y) => A): X => A = new ConstFun(g(value))
		
		override def equals(that :Any) = that match {
			case const :ConstFun[_, _] => (this eq const) || value==const.value
			case _ => false
		}
		
		override def hashCode = value.hashCode
		
		override def toString = s"{_ => $value}"
	}
	
	

	class ComposedFun[@specialized(Fun1ArgTypes) -X, @specialized(Fun1ArgTypes) Y, @specialized(Fun1ResultTypes) +Z](val inner :X=>Y, val outer :Y=>Z)
		extends ComposableFun[X, Z]
	{
		override def apply(x: X): Z = outer(inner(x))

		override def equals(that :Any) = that match {
			case c :ComposedFun[_, _, _] => (c eq this) || c.inner==inner && c.outer==outer
			case _ => false
		}

		override def hashCode = inner.hashCode * 31 + outer.hashCode

		override def toString = ComposedFun.name(nameOf(outer, "f"), nameOf(inner, "g")) //s"<${nameOf(outer, "f")} * ${nameOf(inner, "g")}>"
			
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
		//			def cast[T[A], Z<:X](t :T[Z]) :T[Y]
		
		implicit val <:< : X<:<Y
		//			def =:= : X=:=Y
		
		implicit def cocast[T[+A]] :CastFun[T[X], T[Y]]
		implicit def contracast[T[-A]] :CastFun[T[Y], T[X]]
		
		implicit def covariant[T[+A]] :T[X] <:< T[Y]
		implicit def contravariant[T[-A]] :T[Y] <:< T[X]
		implicit def pair[T[-A, +B]] : T[Y, X] <:< T[X, Y]
		
		
	}
	
	

	
	class UpCast[-X<:Y, +Y] extends CastFun[X, Y] {
		def apply(x :X) :Y = x
		
		@scala.annotation.unspecialized
		override def compose[A](g: (A) => X): (A) => Y = g
		
		@scala.annotation.unspecialized
		override def andThen[A](g: (Y) => A): (X) => A = g
		
		def ident :X => Y = idfun[X]
		
		final implicit val <:< :X<:<Y = implicitly[X<:<X]
		
		final implicit override def cocast[T[+A]] = CastFun[T[X], T[Y]]
		
		final implicit override def contracast[T[-A]] = CastFun[T[Y], T[X]]
		
		final implicit override def covariant[T[+A]] :T[X]<:<T[Y] = implicitly[T[X] <:< T[X]]
		
		final implicit override def contravariant[T[-A]] :T[Y]<:<T[X] = implicitly[T[Y] <:< T[Y]]
		
		final implicit override def pair[T[-A, +B]] :T[Y, X] <:< T[X, Y] = implicitly[T[Y, X] <:< T[Y, X]]
		
		override def toString = s"((_:X) :Y)"
	}
	
	private final class IdentityCast[X] extends UpCast[X, X] with GenericIdentity[X] {
		override def ident :X=>X = this
	}
	
	
	
	
	
	
	

//	@inline final def __=>[T](value :T) = { _ :Any => value }

	class FunctionNamer(override val toString :String) extends AnyVal { name =>

		def apply[@specialized(Fun1ArgTypes) T1, @specialized(Fun1ResultTypes) R](f :T1=>R) :T1 => R = new NamedFun1(toString, f)

		def apply[@specialized(Fun2Arg1Types) T1, @specialized(Fun2Arg2Types) T2, @specialized(Fun2ResultTypes) R](f :(T1, T2)=>R) :(T1, T2) => R =
			new Function2[T1, T2, R]
			{
				override def apply(v1: T1, v2: T2): R = f(v1, v2)

				@unspecialized
				override def curried: (T1) => (T2) => R = Fun(toString)(super.curried)

				@unspecialized
				override def tupled: ((T1, T2)) => R = Fun(s"($toString)")(super.tupled)

				override val toString = name.toString
			}
	}


}
