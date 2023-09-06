package net.noresttherein.sugar.funny

import scala.annotation.unspecialized
import scala.reflect.ClassTag
import scala.Specializable.{Arg, Args, Return}

import net.noresttherein.sugar.extensions.{Function2Extension, Function3Extension}
import net.noresttherein.sugar.funny.ComposableFunUtils.{FunString, InverseCompose, UnknownType, argsString, composedString, funString, typeArgumentName}
import net.noresttherein.sugar.funny.fun.{ComposableFun, ComposableFun2, ComposableFun3}
import net.noresttherein.sugar.funny.fun.specializations.ReturnVal
import net.noresttherein.sugar.reflect
import net.noresttherein.sugar.reflect.prettyprint




/** Factory methods for functions with benefits.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
object fun {
	import specializations._
	//todo: macros

	/** Enforces the type of the passed function expression
	  * to be a [[net.noresttherein.sugar.funny.fun.ComposableFun ComposableFun]].
	  * As `ComposableFun` is a ''Single Abstract Method'' type lacking only the implementation of `apply`, a
	  * function expression will be converted by the compiler where needed:
	  * {{{
	  *     fun[Int] { x => x + x }
	  * }}}
	  * Created function provides a more informative `toString` method and elides some special functions
	  * defined here during composition.
	  * This method serves no other purpose and promptly returns the argument as `X=>Y` to help with type inference.
	  * @return `f`
	  */ //apply[X, Y](f :ComposableFun[X, Y]) :X => Y does not work in Scala 2
	@inline def apply[X] :ComposableFunSAMFactory[X] = new ComposableFunSAMFactory[X] {}

	sealed trait ComposableFunSAMFactory[X] extends Any with Serializable {
		@inline final def apply[Y](f :ComposableFun[X, Y]) :X => Y = f
	}

	/** Enforces the type of the passed function expression
	  * to be a [[net.noresttherein.sugar.funny.fun.ComposableFun2 ComposableFun2]].
	  * As `ComposableFun` is a ''Single Abstract Method'' type lacking only the implementation of `apply`, a
	  * function expression will be converted by the compiler where needed:
	  * {{{
	  *     fun[Int, Int](_ + _)
	  * }}}
	  * Created function provides a more informative `toString` method and elides some special functions
	  * defined here during composition (which is possible through general 2-argument function
	  * [[net.noresttherein.sugar.funny.extensions.Function2Extension extension methods]]).
	  * This method serves no other purpose and promptly returns the argument as `(X, Y) => Z` to help with type inference.
	  * @return `f`
	  */
	@inline def apply[X, Y] :ComposableFun2SAMFactory[X, Y] = new ComposableFun2SAMFactory[X, Y] {}

	sealed trait ComposableFun2SAMFactory[X, Y] extends Any with Serializable {
		@inline final def apply[Z](f :ComposableFun2[X, Y, Z]) :(X, Y) => Z = f
	}

	/** Enforces the type of the passed function expression
	  * to be a [[net.noresttherein.sugar.funny.fun.ComposableFun3 ComposableFun3]].
	  * As `ComposableFun` is a ''Single Abstract Method'' type lacking only the implementation of `apply`, a
	  * function expression will be converted by the compiler where needed:
	  * {{{
	  *     fun[Int, Int, Int](_ + _ + _)
	  * }}}
	  * Created function provides a more informative `toString` method and elides some special functions
	  * defined here during composition (which is possible through general 3-argument function
	  * [[net.noresttherein.sugar.funny.extensions.Function3Extension extension methods]]).
	  * This method serves no other purpose and promptly returns the argument as `(W, X, Y) => Z`
	  * to help with type inference.
	  * @return `f`
	  */
	@inline def apply[W, X, Y] :ComposableFun3SAMFactory[W, X, Y] = new ComposableFun3SAMFactory[W, X, Y] {}

	sealed trait ComposableFun3SAMFactory[W, X, Y] extends Any with Serializable {
		@inline final def apply[Z](f :ComposableFun3[W, X, Y, Z]) :(W, X, Y) => Z = f
	}



	/** Enforces the type of the passed function expression to be a [[net.noresttherein.sugar.funny.fun.PureFun PureFun]].
	  * This is essentially the same as [[net.noresttherein.sugar.funny.fun.apply fun(f)]] except it declares that the
	  * created function throws no exceptions, has no side effects and does not depend on mutable state.
	  * This allows it to be elided when composed with constant functions and those throwing exceptions.
	  * As there is no way to verify these claims, you are on a honor system here.
	  * @return `f`
	  */
	@inline def pure[X, Y](f :PureFun[X, Y]) :X => Y = f


	/** Creates a named function. The string given as the first argument is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function1&gt;'. The second argument is the returned
	  * function itself; however, being a ''SAM'' type, a function expression is automatically converted to the target
	  * function type. Hence, the syntax would be:
	  * {{{
	  *     fun.named[Int]("square") { x => x * x }
	  * }}}
	  * This method simply sets the name on the second argument to the given string and returns it as the standard
	  * function type `X => Y` for better type inference. Note that as `NamedFun` is a class, in a generic context
	  * `f` will not be specialized as subclasses of specialized classes have to extend their erased variant.
	  * In those circumstances wrapping the function with the sister method [[net.noresttherein.sugar.funny.fun.name name]]
	  * might be preferable.
	  * @param name name to use as the textual representation of the given function.
	  * @return `f`
	  */
	@inline def named[X](name :String) = new NamedFunSAM[X](name)

	class NamedFunSAM[X] private[fun] (private val name :String) extends AnyVal {
		def apply[Y](f :NamedFun[X, Y]) :X => Y = { f.rename(name); f }
	}

	/** Creates a named two argument function. The string given as the first argument is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function2&gt;'. The second argument is the returned
	  * function itself; however, being a ''SAM'' type, a function expression is automatically converted to the target
	  * function type. Hence, the syntax would be:
	  * {{{
	  *     fun.named[Int, Int]("square")(_ + _)
	  * }}}
	  * This method simply sets the name on the second argument to the given string and returns it as the standard
	  * function type `(X, Y) => Z` for better type inference. Note that as `NamedFun2` is a class, in a generic context
	  * `f` will not be specialized as subclasses of specialized classes have to extend their erased variant.
	  * In those circumstances wrapping the function with the sister method
	  * [[net.noresttherein.sugar.funny.fun.name2 name2]] might be preferable.
	  * @param name name to use as the textual representation of the given function.
	  * @return `f`
	  */
	@inline def named2[X, Y](name :String) = new NamedFun2SAM[X, Y](name)

	class NamedFun2SAM[X, Y] private[fun] (private val name :String) extends AnyVal {
		def apply[Z](f :NamedFun2[X, Y, Z]) :(X, Y) => Z = { f.rename(name); f }
	}

	/** Creates a named three argument function. The string given as the first argument is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function3&gt;'. The second argument is the returned
	  * function itself; however, being a ''SAM'' type, a function expression is automatically converted to the target
	  * function type. Hence, the syntax would be:
	  * {{{
	  *     fun.named[Int, Int, Int]("square")(_ + _ + _)
	  * }}}
	  * This method simply sets the name on the second argument to the given string and returns it as the standard
	  * function type `(W, X, Y) => Z` for better type inference. Note that as `NamedFun3` is a class,
	  * in a generic context `f` will not be specialized as subclasses of specialized classes have to extend
	  * their erased variant. In those circumstances wrapping the function with the sister method
	  * [[net.noresttherein.sugar.funny.fun.name2 name2]] might be preferable.
	  * @param name name to use as the textual representation of the given function.
	  * @return `f`
	  */
	@inline def named3[W, X, Y](name :String) = new NamedFun3SAM[W, X, Y](name)

	class NamedFun3SAM[W, X, Y] private[fun] (private val name :String) extends AnyVal {
		def apply[Z](f :NamedFun3[W, X, Y, Z]) :(W, X, Y) => Z = { f.rename(name); f }
	}

	/** Wraps the given function in a decorator returning the given name from its `toString` method.
	  * @see [[net.noresttherein.sugar.funny.fun.named]]
	  */
	@inline def name(name :String) = new NamedFunWrapper(name)

	class NamedFunWrapper private[fun] (private val name :String) extends AnyVal {
		@inline def apply[@specialized(Arg) X, @specialized(Return) Y](f :X => Y) :X => Y =
			new ComposableFun[X, Y] {
				override def apply(x :X) :Y = f(x)
				override def toString = name
			}
		@inline def apply[@specialized(Args) X, @specialized(Args) Y, @specialized(Return) Z]
			             (f :(X, Y) => Z) :(X, Y) => Z =
			new ComposableFun2[X, Y, Z] {
				override def apply(x :X, y :Y) :Z = f(x, y)
				override def toString = name
			}
		@inline def apply[W, X, Y, Z](f :(W, X, Y) => Z) :(W, X, Y) => Z =
			new ComposableFun3[W, X, Y, Z] {
				override def apply(w :W, x :X, y :Y) :Z = f(w, x, y)
				override def toString = name
			}
	}


	/** Equivalent to `scala.identity[X]`, but is specialized and overrides `compose` (and `andThen`)
	  * for reduced overhead of function composition. Additionally, it provides a more informative `toString` output.
	  */
	@inline def ident[@specialized(Arg) X] :X => X = new Identity[X] {}

	/** A constant function equivalent to `(_:X) => value`, but with a more helpful `toString`,
	  * and performing reduction on composition. For a `f = const[X](value)`, where `value :Y`, and any `g :Y => O`,
	  * `f endThen g` eagerly evaluates to `const[X](g(value))`. Additionally, for any `h :A => X`
	  * implementing [[net.noresttherein.sugar.funny.fun.PureFun PureFun]], `f compose h` evaluates simply
	  * to `const[A](value)`.
	  *
	  * This method returns only a factory object which `apply` method accepts the constant. This is to separate the
	  * declarations of argument and return type parameters - the latter can be inferred by the compiler,
	  * while the former can't. While not providing the argument type here will result in a function `Any => Y`,
	  * which will serve for any type `X`, it will not be specialized.
	  */
	@inline def const[@specialized(Arg) X] :ConstFunFactory[X] = new ConstFunFactory[X] {}

	/** A constant function equivalent to `(_:X, _:Y) => value`, but with a more helpful `toString`,
	  * and performing reduction on composition. For a `f = const2[X, Y](value)`, where `value :Z`, and any `g :Z => O`,
	  * `f endThen g` eagerly evaluates to `const2[X, Y](g(value))`.
	  *
	  * This method returns only a factory object which `apply` method accepts the constant. This is to separate the
	  * declarations of argument and return type parameters - the latter can be inferred by the compiler,
	  * while the former can't. While not providing the argument type here will result in a function `(Any, Any) => Z`,
	  * which will serve for any type `X`, it will not be specialized.
	  */
	@inline def const2[@specialized(Args) X, @specialized(Args) Y] :ConstFun2Factory[X, Y] =
		new ConstFun2Factory[X, Y] {}

	/** A constant function equivalent to `(_:W, _:X, _:Y) => value`, but with a more helpful `toString`,
	  * and performing reduction on composition. For a `f = const3[W, X, Y](value)`, where `value :Z`,
	  * and any `g :Z => O`, `f endThen g` eagerly evaluates to `const3[W, X, Y](g(value))`.
	  *
	  * This method returns only a factory object which `apply` method accepts the constant. This is to separate the
	  * declarations of argument type and return type parameters - the latter can be inferred by the compiler,
	  * while the former can't. Functions of three arguments are not specialized, so it is also possible to skip
	  * providing any type arguments, in order to obtain a universal `(Any, Any, Any) => Z`.
	  */
	@inline def const3[W, X, Y] :ConstFun3Factory[W, X, Y] =
		new ConstFun3Factory[W, X, Y] {}


	/** A function throwing the exception returned for its argument by the passed constructor function.
	  * Provides an informative textual representation and performs reduction during composition, ignoring any
	  * function passed to its `andThen`, returning itself instead.
	  */
	@inline def throwing[X](exception :X => Throwable) :X => Nothing = new ThrowingFunction(exception)

	/** A function throwing the exception returned for its arguments by the passed constructor function.
	  * Provides an informative textual representation and performs reduction during composition, ignoring any
	  * function passed to its `andThen`, returning itself instead.
	  */
	@inline def throwing[X, Y](exception :(X, Y) => Throwable) :(X, Y) => Nothing = new ThrowingFunction2(exception)

	/** A function throwing the exception returned for its arguments by the passed constructor function.
	  * Provides an informative textual representation and performs reduction during composition, ignoring any
	  * function passed to its `andThen`, returning itself instead.
	  */
	@inline def throwing[W, X, Y](exception :(W, X, Y) => Throwable) :(W, X, Y) => Nothing =
		new ThrowingFunction3(exception)

	/** A function which always throws the given exception. Provides informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.sugar.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	@inline def throwing(exception :Throwable) :Any => Nothing = new Throw(exception)

	/** A function which always throws the given exception. Provides informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.sugar.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	@inline def throwing2(exception :Throwable) :(Any, Any) => Nothing = new Throw2(exception)

	/** A function which always throws the given exception. Provides informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.sugar.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	@inline def throwing3(exception :Throwable) :(Any, Any, Any) => Nothing = new Throw3(exception)

	/** A function which always throws the exception provided as the type parameter. The `T` must be a concrete class
	  * implementing throwable and providing a default no-argument constructor or this method will throw an
	  * `InstantiationException`. Returned function provides an informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.sugar.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	@inline def throwing[T <: Throwable :ClassTag] :Any => Nothing =
		new Throw(implicitly[ClassTag[T]].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable])


	/**  A simple factory object which `apply` method creates functions which always return the same value.
	  *  Returned by [[net.noresttherein.sugar.funny.fun.const fun.const]], it separates the argument
	  *  and value type parameters, so the latter can be inferred from the provided value.
	  */
	trait ConstFunFactory[@specialized(Arg) X] extends Any {
		/** Creates a function which always returns `const`. */
		@inline final def apply[@specialized(ReturnVal) Y](const :Y) :X => Y = new ConstFun[X, Y](const)
	}

	/**  A simple factory object which `apply` method creates two argument functions which always return the same value.
	  *  Returned by [[net.noresttherein.sugar.funny.fun.const2 fun.const2]], it separates the argument
	  *  and value type parameters, so the latter can be inferred from the provided value.
	  */
	trait ConstFun2Factory[@specialized(Args) X, @specialized(Args) Y] extends Any {
		/** Creates a function which always returns `const`. */
		@inline final def apply[@specialized(ReturnVal) Z](const :Z) :(X, Y) => Z = new ConstFun2[X, Y, Z](const)
	}

	/**  A simple factory object which `apply` method creates three argument functions which always return
	  *  the same value. Returned by [[net.noresttherein.sugar.funny.fun.const3 fun.const3]], it separates the argument
	  *  and value type parameters, so the latter can be inferred from the provided value.
	  */
	trait ConstFun3Factory[W, X, Y] extends Any {
		/** Creates a function which always returns `const`. */
		@inline final def apply[Z](const :Z) :(W, X, Y) => Z = new ConstFun3[W, X, Y, Z](const)
	}



	/** Declaration of type groups usable as arguments to scala `@specialized` annotation. */
	@SerialVersionUID(Ver)
	object specializations {
		/** Types for which `Function1`'s result type is specialized, minus `Unit`. */
		final val ReturnVal = new Specializable.Group(Boolean, Int, Float, Long, Double)

		@SerialVersionUID(Ver)
		private[fun] class SpecializedType[@specialized(Return) T] {

			@inline final override def equals(that :Any) :Boolean = getClass == that.getClass

			override def toString :String = {
				val cls = getClass
				if (cls == AnyT.getClass) "_"
				else if (cls == IntT.getClass) "Int"
				else if (cls == LongT.getClass) "Long"
				else if (cls == DoubleT.getClass) "Double"
				else if (cls == BooleanT.getClass) "Boolean"
				else if (cls == FloatT.getClass) "Float"
				else if (cls == UnitT.getClass) "()"
				else UnknownType
			}
		}

		private[this] final val UnitT = new SpecializedType[Unit]
		private[this] final val BooleanT = new SpecializedType[Boolean]
		private[this] final val IntT = new SpecializedType[Int]
		private[this] final val FloatT = new SpecializedType[Float]
		private[this] final val LongT = new SpecializedType[Long]
		private[this] final val DoubleT = new SpecializedType[Double]
		private[this] final val AnyT = new SpecializedType[Any]
	}



	/** A base interface for 'nice' functions defined here. Overrides `compose` and `andThen` to attain three goals:
	  *  - a `@specialized` instance for the composed function;
	  *  - eliding one of the members if an identity, constant or exception throwing function is encountered.
	  *  - providing a more helpful `toString` implementation.
	  * @see [[net.noresttherein.sugar.funny.fun]]
	  * @define reflectedTypeNameInfo If this function extends $Fun specialized for a value type, the Scala name
	  *                               of the type is returned. Otherwise it tries to determine the provided
	  *                               type parameter by reflection and return a shortened name of the class
	  *                               (with package names replaced with their first letter). If the type cannot
	  *                               be determined, or is `Any`/`java.lang.Object`, `"_"` is returned.
	  *                               This will be the case for non-specialized functions (including those with
	  *                               only one specializable type parameter) or generic functions which actual
	  *                               type arguments are declared as type parameters of some enclosing class.
	  *                               Subclasses are free to override this method with a more efficient
	  *                               or detailed version.
	  * @define Fun `ComposableFun`
	  */
	trait ComposableFun[@specialized(Arg) -X, @specialized(Return) +Y] extends (X => Y) with Serializable {

		override def compose[A](g :A => X) :A => Y = g match {
			case f :ComposableFun[A, X] => this compose_: f
			case _ => new ComposedFun(g, this)
		}

		def compose[A, B](g :(A, B) => X) :(A, B) => Y = g match {
			case f :ComposableFun2[A, B, X] => this compose_: f
			case _ => new ComposedFun2(g, this)
		}
		def compose[A, B, C](g :(A, B, C) => X) :(A, B, C) => Y = g match {
			case f :ComposableFun3[A, B, C, X] => this compose_: f
			case _ => new ComposedFun3(g, this)
		}

		override def andThen[A](g :Y => A) :X => A = g match {
			case f :ComposableFun[Y, A] => this andThen_: f
			case _ => new ComposedFun(this, g)
		}

		protected[fun] def compose_:[@specialized(ReturnVal) Z](g :Y => Z) :X => Z =
			new ComposedFun(this, g)

		protected[fun] def andThen_:[@specialized(Arg) W](g :W => X) :W => Y =
			new ComposedFun(g, this)

		protected[fun] def andThen_:[@specialized(Args) V, @specialized(Args) W](g :(V, W) => X) :(V, W) => Y =
			new ComposedFun2(g, this)

		protected[fun] def andThen_:[U, V, W](g :(U, V, W) => X) :(U, V, W) => Y =
			new ComposedFun3(g, this)

		/** Double dispatch target of `andThen` to ensure proper specialization. */
		protected[fun] def andThen_:[@specialized(Arg) W](const :X) :W => Y = new ConstFun[W, Y](apply(const))

		/** Double dispatch target of `andThen` to ensure proper specialization. */
		protected[fun] def andThen2_:[@specialized(Args) V, @specialized(Args) W](const :X) :(V, W) => Y =
			new ConstFun2[V, W, Y](apply(const))


		def canEqual(that :Any) :Boolean = false

		/** Uses java reflection to determine the type of the `i`-th type argument given to this trait by the concrete
		  * class of this object.
		  */
		private[this] def typeArgument(i :Int, spec :SpecializedType[_]) :String = {
			val name = spec.toString
			if (name.length > 1)
				name
			else typeArgumentName(getClass, i, classOf[ComposableFun[_, _]])
		}
		private[this] def domain :SpecializedType[X] = new SpecializedType[X]
		private[this] def range  :SpecializedType[Y] = new SpecializedType[Y]

		/** String representation of the argument type.
		  * $reflectedTypeNameInfo
		  */
		def domainString :String = typeArgument(0, domain)

		/** String representation of the result type.
		  * $reflectedTypeNameInfo
		  */
		def rangeString :String = typeArgument(1, range)

		/** The default `toString` implementation, returning `"${domainString}=>${rangeString}"`. */
		def typeString :String = funString(domainString, rangeString)

		/** By default returns `"${domainString}=>${rangeString}"`. Some concrete classes override it with information
		  * about the actual nature of the function.
		  */
		override def toString :String = typeString
	}



	/** A base interface for 'nice' two argument functions defined here. Defines  `andThen` to attain three goals:
	  *  - `@specialized` function composition,
	  *  - eliding one of the members if an identity, constant or exception throwing function is encountered,
	  *  - providing a more helpful `toString` implementation.
	  * @see [[net.noresttherein.sugar.funny.fun]]
	  * @define reflectedTypeNameInfo If this function extends $Fun specialized for a value type, the Scala name
	  *                               of the type is returned. Otherwise it tries to determine the provided
	  *                               type parameter by reflection and return a shortened name of the class
	  *                               (with package names replaced with their first letter). If the type cannot
	  *                               be determined, or is `Any`/`java.lang.Object`, `"_"` is returned.
	  *                               This will be the case for non-specialized functions (including those with
	  *                               only one specializable type parameter) or generic functions which actual
	  *                               type arguments are declared as type parameters of some enclosing class.
	  *                               Subclasses are free to override this method with a more efficient
	  *                               or detailed version.
	  * @define Fun                   `ComposableFun3`
	  */
	trait ComposableFun2[@specialized(Args) -A, @specialized(Args) -B, @specialized(ReturnVal) +Y]
		extends ((A, B) => Y) with Serializable
	{
		def andThen[Z](f :Y => Z) :(A, B) => Z = f match {
			case g :ComposableFun[Y, Z] => this andThen_: g
			case _ => new ComposedFun2(this, f)
		}
		protected[fun] def compose_:[@specialized(ReturnVal) Z](f :Y => Z) :(A, B) => Z =
			new ComposedFun2(this, f)


		def canEqual(that :Any) :Boolean = false

		/** Uses java reflection to determine the type of the `i`-th type argument given to this trait by the concrete
		  * class of this object.
		  */
		private[this] def typeArgument(i :Int, spec :SpecializedType[_]) :String = {
			val name = spec.toString
			if (name.length > 1)
				name
			else typeArgumentName(getClass, i, classOf[ComposableFun2[_, _, _]])
		}

		private[this] def arg1  :SpecializedType[A] = new SpecializedType[A]
		private[this] def arg2  :SpecializedType[B] = new SpecializedType[B]
		private[this] def range :SpecializedType[Y] = new SpecializedType[Y]

		/** String representation of the first argument type.
		  * $reflectedTypeNameInfo
		  */
		def arg1String :String = typeArgument(0, arg1)

		/** String representation of the second argument type.
		  * $reflectedTypeNameInfo
		  */
		def arg2String :String = typeArgument(1, arg2)

		/** String representation of the reflected parameter types. */
		def domainString :String = argsString(this)

		/** String representation of the result type.
		  * $reflectedTypeNameInfo
		  */
		def rangeString :String = typeArgument(3, range)

		/** The default `toString` implementation, returning `"(${arg1String},${arg2String})=>${rangeString}"`. */
		def typeString :String = funString(domainString, rangeString)

		/** By default returns `"(${arg1String},${arg2String})=>${rangeString}"`.
		  * Some concrete classes override it with information about the actual nature of the function.
		  */
		override def toString :String = typeString
	}



	/** A base interface for 'nice' three argument functions defined here. Defines  `andThen` to attain two goals:
	  *  - eliding one of the members if an identity, constant or exception throwing function is encountered,
	  *  - providing a more helpful `toString` implementation.
	  * @see [[net.noresttherein.sugar.funny.fun]]
	  * @define reflectedTypeNameInfo If this function extends $Fun specialized for a value type, the Scala name
	  *                               of the type is returned. Otherwise it tries to determine the provided
	  *                               type parameter by reflection and return a shortened name of the class
	  *                               (with package names replaced with their first letter). If the type cannot
	  *                               be determined, or is `Any`/`java.lang.Object`, `"_"` is returned.
	  *                               This will be the case for non-specialized functions (including those with
	  *                               only one specializable type parameter) or generic functions which actual
	  *                               type arguments are declared as type parameters of some enclosing class.
	  *                               Subclasses are free to override this method with a more efficient
	  *                               or detailed version.
	  * @define Fun `ComposableFun3`
	  */
	trait ComposableFun3[-A, -B, -C, +Y] extends ((A, B, C) => Y) with Serializable {
		def andThen[Z](f :Y => Z) :(A, B, C) => Z = f match {
			case g :ComposableFun[Y, Z] => this andThen_: g
			case _ => new ComposedFun3(this, f)
		}
		protected[fun] def compose_:[Z](f :Y => Z) :(A, B, C) => Z =
			new ComposedFun3(this, f)


		def canEqual(that :Any) :Boolean = false

		/** Uses java reflection to determine the type of the `i`-th type argument given to this trait by the concrete
		  * class of this object.
		  */
		private[this] def typeArgument(i :Int, spec :SpecializedType[_]) :String = {
			val name = spec.toString
			if (name.length > 1)
				name
			else typeArgumentName(getClass, i, classOf[ComposableFun3[_, _, _, _]])
		}

		private[this] def arg1  :SpecializedType[A] = new SpecializedType[A]
		private[this] def arg2  :SpecializedType[B] = new SpecializedType[B]
		private[this] def arg3  :SpecializedType[C] = new SpecializedType[C]
		private[this] def range :SpecializedType[Y] = new SpecializedType[Y]

		/** String representation of the first argument type.
		  * $reflectedTypeNameInfo
		  */
		def arg1String :String = typeArgument(0, arg1)

		/** String representation of the second argument type.
		  * $reflectedTypeNameInfo
		  */
		def arg2String :String = typeArgument(1, arg2)

		/** String representation of the third argument type.
		  * $reflectedTypeNameInfo
		  */
		def arg3String :String = typeArgument(3, arg3)

		/** String representation of the reflected parameter types. */
		def domainString :String = argsString(this)

		/** String representation of the result type.
		  * $reflectedTypeNameInfo
		  */
		def rangeString :String = typeArgument(4, range)

		/** The default `toString` implementation, returning `"(${arg1String},${arg2String},${arg3String})=>${rangeString}"`. */
		def typeString :String = funString(domainString, rangeString)

		/** By default returns `"(${arg1String},${arg2String},${arg3String})=>${rangeString}"`.
		  * Some concrete classes override it with information about the actual nature of the function.
		  */
		override def toString :String = typeString
	}



	/** Marker trait for functions which are not expected to throw any exceptions, do not have side effects
	  * and do not depend on mutable state. They can be elided when composed with constant or throwing functions.
	  */
	trait PureFun[@specialized(Arg) -X, @specialized(ReturnVal) +Y] extends ComposableFun[X, Y]


	/** A base class for functions with a predefined `toString` implementation. It exists solely for the purpose
	  * of simplified syntax of [[net.noresttherein.sugar.funny.fun.named fun.named]]. Non-anonymous function
	  * implementations which desire to override `toString` should instead extend the `ComposableFun` supertype directly.
	  */
	abstract class NamedFun[@specialized(Arg) -X, @specialized(Return) +Y] extends ComposableFun[X, Y] {
		def this(name :String) = { this(); this.name = name }

		@volatile private[this] var name :String = _

		private[fun] def rename(name :String) :Unit = {
			this.name = name
			java.lang.invoke.VarHandle.releaseFence()
		}

		override def toString :String = {
			var res = name
			if (res == null) {
				res = typeString
				name = res
			}
			res
		}
	}


	/** A base class for functions with a predefined `toString` implementation. It exists solely for the purpose
	  * of simplified syntax of [[net.noresttherein.sugar.funny.fun.named fun.named]]. Non-anonymous function
	  * implementations which desire to override `toString` should instead extend the `ComposableFun` supertype directly.
	  */
	abstract class NamedFun2[@specialized(Args) -A, @specialized(Args) -B, @specialized(Return) +Y]
		extends ComposableFun2[A, B, Y]
	{
		def this(name :String) = { this(); this.name = name }

		@volatile private[this] var name :String = _

		private[fun] def rename(name :String) :Unit = {
			this.name = name
			java.lang.invoke.VarHandle.releaseFence()
		}

		override def toString :String = {
			var res = name
			if (res == null) {
				res = typeString
				name = res
			}
			res
		}
	}


	/** A base class for functions with a predefined `toString` implementation. It exists solely for the purpose
	  * of simplified syntax of [[net.noresttherein.sugar.funny.fun.named fun.named]]. Non-anonymous function
	  * implementations which desire to override `toString` should instead extend the `ComposableFun` supertype directly.
	  */
	abstract class NamedFun3[-A, -B, -C, +Y] extends ComposableFun3[A, B, C, Y] {
		def this(name :String) = { this(); this.name = name }

		@volatile private[this] var name :String = _

		private[fun] def rename(name :String) :Unit = {
			this.name = name
			java.lang.invoke.VarHandle.releaseFence()
		}

		override def toString :String = {
			var res = name
			if (res == null) {
				res = typeString
				name = res
			}
			res
		}
	}



	/** An identity function implementation with overridden `compose` and `andThen` methods so it is elided
	  * on composition. Additionally, provides an indicative `toString` implementation.
	  */
	trait Identity[@specialized(Arg) X] extends ComposableFun[X, X] with PureFun[X, X] {
		final override def apply(x :X) :X = x

		@unspecialized override def compose[A](g :A => X) :A => X = g
		@unspecialized override def compose[A, B](g :(A, B) => X) :(A, B) => X = g
		@unspecialized override def andThen[A](g :X => A) :X => A = g
		@unspecialized override def compose[A, B, C](g :(A, B, C) => X) :(A, B, C) => X = g

		@unspecialized protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :X => Z) :X => Z = g
		@unspecialized protected[fun] override def andThen_:[@specialized(Arg) W](g :W => X) :W => X = g
		@unspecialized protected[fun] override def andThen_:[@specialized(Args) V, @specialized(Args) W]
		                                                    (g :(V, W) => X) :(V, W) => X = g
		@unspecialized protected[fun] override def andThen_:[U, V, W](g :(U, V, W) => X) :(U, V, W) => X = g
//		protected[fun] override def andThen_:[W](const :X) :W => X = super.andThen_:(const)

		override def equals(that :Any) :Boolean = that match {
			case id :Identity[_] => (id eq this) || canEqual(id) && id.canEqual(this) && {
				val domain = domainString
				domain.length > 1 && domain == id.domainString
			}
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Identity[_]]
		override def hashCode :Int = domainString.hashCode

		override def toString :String = "Identity[" + domainString + "]"
	}



	@SerialVersionUID(Ver)
	private class ConstFun[@specialized(Arg) -X, @specialized(Return) +Y](result :Y)
		extends ComposableFun[X, Y] with PureFun[X, Y]
	{
		final override def apply(x :X) :Y = result

		@unspecialized override def compose[A](g :A => X) :A => Y = g andThen_: this
		@unspecialized override def compose[A, B](g :(A, B) => X) = g andThen_: this
		@unspecialized override def compose[A, B, C](g :(A, B, C) => X) = g andThen_: this

		override def andThen[A](g :Y => A) :X => A = g compose_: this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Y => Z) :X => Z =
			try {
				new ConstFun(g(result))
			} catch {
				case _ :Throwable => super.compose_:(g)
			}
		protected[fun] override def andThen_:[@specialized(Arg) W](g :W => X) :W => Y = g match {
			case _ :ConstFun[_, _] => new ConstFun(result)
			case _ => new ComposedFun(g, this)
		}
		protected[fun] override def andThen_:[@specialized(Args) V, @specialized(Args) W](g :(V, W) => X) :(V, W) => Y =
			g match {
				case _ :ConstFun2[_, _, _] => new ConstFun2(result)
				case _ => new ComposedFun2(g, this)
			}
		protected[fun] override def andThen_:[U, V, W](g :(U, V, W) => X) :(U, V, W) => Y = g match {
			case _ :ConstFun3[_, _, _, _] => new ConstFun3(result)
			case _ => new ComposedFun3(g, this)
		}

		override def toString :String = {
			val x = domainString
			if (x == UnknownType)
				funString(domainString, result.toString)
			else
				funString(argsString("_:" + x), result.toString)
		}
	}


	@SerialVersionUID(Ver)
	private class ConstFun2[@specialized(Args) -A, @specialized(Args) -B, @specialized(ReturnVal) +Y](result :Y)
		extends ComposableFun2[A, B, Y]
	{
		final override def apply(v1 :A, v2 :B) :Y = result

		override def andThen[Z](g :Y => Z) :(A, B) => Z = g compose_: this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Y => Z) :(A, B) => Z =
			try {
				new ConstFun2(g(result))
			} catch {
				case _ :Throwable => super.compose_:(g)
			}

		override def toString :String = {
			val x1 = arg1String
			val x2 = arg2String
			if (x1 == UnknownType && x2 == UnknownType)
				funString(domainString, result.toString)
			else
				funString(argsString("_:" + x1, "_:" + x2), result.toString)
		}
	}


	@SerialVersionUID(Ver)
	private class ConstFun3[-A, -B, -C, +Y](result :Y) extends ComposableFun3[A, B, C, Y] {
		final override def apply(v1 :A, v2 :B, v3 :C) :Y = result

		override def andThen[Z](g :Y => Z) :(A, B, C) => Z = g compose_: this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Y => Z) :(A, B, C) => Z =
			try {
				new ConstFun3(g(result))
			} catch {
				case _ :Throwable => super.compose_:(g)
			}

		override def toString :String = {
			val x1 = arg1String
			val x2 = arg2String
			val x3 = arg3String
			if (x1 == UnknownType && x2 == UnknownType && x3 == UnknownType)
				funString(domainString, result.toString)
			else
				funString(argsString("_:" + x1, "_:" + x2, "_:" + x3), result.toString)
		}
	}



	@SerialVersionUID(Ver)
	private class ThrowingFunction[@specialized(Arg) -X](exception :X => Throwable) extends ComposableFun[X, Nothing] {
		override def apply(x :X) :Nothing = throw exception(x)

		override def compose[A](g :A => X) :A => Nothing = g andThen_: this
		override def compose[A, B](g :(A, B) => X) = g andThen_: this
		override def compose[A, B, C](g :(A, B, C) => X) = g andThen_: this

		override def andThen[A](g :Nothing => A) :X => A = this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Nothing => Z) :X => Z = this
		protected[fun] override def andThen_:[@specialized(Arg) W](g :W => X) :W => Nothing =
			new ThrowingFunction(g andThen exception)

		protected[fun] override def andThen_:[@specialized(Args) V, @specialized(Args) W](g :(V, W) => X) =
			new ThrowingFunction2((v :V, w :W) => exception(g(v, w)))

		protected[fun] override def andThen_:[U, V, W](g :(U, V, W) => X) =
			new ThrowingFunction3((u :U, v :V, w :W) => exception(g(u, v, w)))

		override def rangeString = "Nothing"
	}


	@SerialVersionUID(Ver)
	private class ThrowingFunction2[@specialized(Args) -A, @specialized(Args) -B](exception :(A, B) => Throwable)
		extends ComposableFun2[A, B, Nothing]
	{
		override def apply(v1 :A, v2 :B) :Nothing = throw exception(v1, v2)

		override def andThen[Z](g :Nothing => Z) :(A, B) => Z = this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Nothing => Z) :(A, B) => Z = this

		override def rangeString = "Nothing"
	}


	@SerialVersionUID(Ver)
	private class ThrowingFunction3[-A, -B, -C](exception :(A, B, C) => Throwable)
		extends ComposableFun3[A, B, C, Nothing]
	{
		override def apply(v1 :A, v2 :B, v3 :C) :Nothing = throw exception(v1, v2, v3)

		override def andThen[Z](g :Nothing => Z) :(A, B, C) => Z = this

		protected[fun] override def compose_:[Z](g :Nothing => Z) :(A, B, C) => Z = this

		override def rangeString = "Nothing"
	}



	@SerialVersionUID(Ver)
	private class Throw[-X](exception :Throwable) extends ComposableFun[X, Nothing] {
		override def apply(x :X) :Nothing = throw exception

		override def compose[A](g :A => X) :A => Nothing = g andThen_: this

		override def andThen[A](g :Nothing => A) :X => A = this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Nothing => Z) :X => Z = this

		protected[fun] override def andThen_:[@specialized(Arg) W](g :W => X) :W => Nothing =
			if (g.isInstanceOf[PureFun[_, _]]) new Throw[W](exception)
			else new ComposedFun(g, this)

		override def rangeString :String = "Nothing"
		override def toString    :String = "throw " + exception
	}

	@SerialVersionUID(Ver)
	private class Throw3[-A, -B, -C](exception :Throwable) extends ComposableFun3[A, B, C, Nothing] {
		override def apply(v1 :A, v2 :B, v3 :C) :Nothing = throw exception

		override def andThen[Z](g :Nothing => Z) :(A, B, C) => Z = this

		protected[fun] override def compose_:[Z](g :Nothing => Z) :(A, B, C) => Z = this

		override def rangeString :String = "Nothing"
		override def toString    :String = "throw " + exception
	}

	@SerialVersionUID(Ver)
	private class Throw2[-A, -B](exception :Throwable) extends ComposableFun2[A, B, Nothing] {
		override def apply(v1 :A, v2 :B) :Nothing = throw exception

		override def andThen[Z](g :Nothing => Z) :(A, B) => Z = this

		protected[fun] override def compose_:[@specialized(ReturnVal) Z](g :Nothing => Z) :(A, B) => Z = this

		override def rangeString :String = "Nothing"
		override def toString    :String = "throw " + exception
	}
}




@SerialVersionUID(Ver)
private class ComposedFun[@specialized(Arg) -X, @specialized(Arg) Y, @specialized(Return) +Z](f1 :X => Y, f2 :Y => Z)
	extends ComposableFun[X, Z]
{
	override def apply(x :X) :Z = f2(f1(x))

	@unspecialized
	override def compose[A](g :A => X) :A => Z = f2 compose (f1 compose g)
	override def compose[A, B](g :(A, B) => X) = g andThen f1 andThen f2
	override def compose[A, B, C](g :(A, B, C) => X) = g andThen f1 andThen f2

	@unspecialized
	override def andThen[A](g :Z => A) :X => A = f1 andThen (f2 andThen g)

	def inner = f1
	def outer = f2
	override def equals(that :Any) :Boolean = that match {
		case c :ComposedFun[_, _, _] => (c eq this) || c.inner==inner && c.outer==outer
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedFun[_, _, _]]
	override def hashCode :Int = inner.hashCode * 31 + outer.hashCode

	override def toString :String =
		(outer, inner) match {
			case (f :ComposableFun[_, _], g :ComposableFun[_, _]) =>
				val innerS = g.toString
				val outerS = f.toString
				val outerTypePrefix = f.domainString
				if (innerS.endsWith(g.rangeString) && outerS.startsWith(outerTypePrefix))
					funString(innerS, outerS.substring(outerTypePrefix.length))
				else
					composedString(innerS, outerS)

			case (f :ComposableFun[_, _], _) if inner.toString == "<function1>" =>
				val outerS = f.toString
				if (outerS.startsWith(f.domainString))
					funString(UnknownType, outerS)
				else
					composedString(FunString, outerS)
			case (_, g :ComposableFun[_, _]) if outer.toString == "<function1>" =>
				val innerS = g.toString
				if (innerS.endsWith(g.rangeString))
					funString(innerS, UnknownType)
				else
					composedString(innerS, FunString)
			case _ =>
				composedString(inner.toString, outer.toString)
		}

}




@SerialVersionUID(Ver)
private class ComposedFun2[@specialized(Args) -A, @specialized(Args) -B, @specialized(ReturnVal) Y, @specialized(ReturnVal) Z]
                          (f1 :(A, B) => Y, f2 :Y => Z)
	extends ComposableFun2[A, B, Z]
{
	override def apply(v1 :A, v2 :B) :Z = f2(f1(v1, v2))

	@unspecialized
	override def andThen[Z2](g :Z => Z2) :(A, B) => Z2 = f1 match {
		case h :ComposableFun2[_, _, _] => h andThen (f2 andThen g)
		case _ => new ComposedFun2(f1, f2 andThen g)
	}

	private def inner = f1
	private def outer = f2
	override def equals(that :Any) :Boolean = that match {
		case c :ComposedFun2[_, _, _, _] =>
			(c eq this) || c.canEqual(this) && c.inner == inner && c.outer == outer
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedFun2[_, _, _, _]]
	override def hashCode :Int = inner.hashCode * 31 + outer.hashCode

	override def toString :String =
		(outer, inner) match {
			case (f :ComposableFun2[_, _, _], g :ComposableFun[_, _]) =>
				val innerS          = g.toString
				val outerS          = f.toString
				val outerTypePrefix = argsString(f)
				if (innerS.endsWith(g.rangeString) && outerS.startsWith(outerTypePrefix))
					funString(innerS, outerS.substring(outerTypePrefix.length))
				else
					composedString(innerS, outerS)
			case (f :ComposableFun2[_, _, _], _) if inner.toString == "<function1>" =>
				val outerS = f.toString
				if (outerS.startsWith(argsString(f)))
					funString(UnknownType, outerS)
				else
					composedString(FunString, outerS)
			case (_, g :ComposableFun2[_, _, _]) if outer.toString == "<function1>" =>
				val innerS = g.toString
				if (innerS.endsWith(g.rangeString))
					funString(innerS, UnknownType)
				else
					composedString(innerS, FunString)
			case _ =>
				composedString(inner.toString, outer.toString)
		}
}




@SerialVersionUID(Ver)
private class ComposedFun3[-A, -B, -C, Y, Z](f1 :(A, B, C) => Y, f2 :Y => Z) extends ComposableFun3[A, B, C, Z] {
	override def apply(v1 :A, v2 :B, v3 :C) :Z = f2(f1(v1, v2, v3))

	@unspecialized
	override def andThen[Z2](g :Z => Z2) :(A, B, C) => Z2 = f1 match {
		case h :ComposableFun3[_, _, _, _] => h andThen (f2 andThen g)
		case _ => new ComposedFun3(f1, f2 andThen g)
	}

	private def inner = f1
	private def outer = f2
	override def equals(that :Any) :Boolean = that match {
		case c :ComposedFun3[_, _, _, _, _] =>
			(c eq this) || c.canEqual(this) && c.inner == inner && c.outer == outer
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedFun3[_, _, _, _, _]]
	override def hashCode :Int = inner.hashCode * 31 + outer.hashCode

	override def toString :String =
		(outer, inner) match {
			case (f :ComposableFun3[_, _, _, _], g :ComposableFun[_, _]) =>
				val innerS          = g.toString
				val outerS          = f.toString
				val outerTypePrefix = argsString(this)
				if (innerS.endsWith(g.rangeString) && outerS.startsWith(outerTypePrefix))
					funString(innerS, outerS.substring(outerTypePrefix.length))
				else
					funString(innerS, InverseCompose, outerS)
			case (f :ComposableFun3[_, _, _, _], _) if inner.toString == "<function1>" =>
				val outerS = f.toString
				if (outerS.startsWith(argsString(f)))
					funString(UnknownType, outerS)
				else
					composedString(FunString, outerS)
			case (_, g :ComposableFun[_, _]) if f1.toString == "<function1>" =>
				val innerS = g.toString
				if (innerS.endsWith("=>" + g.rangeString))
					funString(innerS, UnknownType)
				else
					composedString(innerS, FunString)
			case _ =>
				composedString(f1.toString, f2.toString)
		}
}






private object ComposableFunUtils {
	final val UnknownType = "?"
	final val FunString   = "?=>?"
	final val InverseCompose = ">>"

	@inline def composedString(args :String, returns :String) :String = funString(args, InverseCompose, returns)
	@inline def funString(args :String, returns :String) :String = funString(args, "=>", returns)
	@inline def funString(args :String, arrow :String, returns :String) :String = args + arrow + returns
	@inline def argString(f :ComposableFun[_, _]) :String = argsString(f.domainString)
	@inline def argsString(f :ComposableFun2[_, _, _]) :String = argsString(f.arg1String, f.arg2String)
	@inline def argsString(f :ComposableFun3[_, _, _, _]) :String = argsString(f.arg1String, f.arg2String, f.arg3String)
	@inline def argsString(x :String) :String = x
	@inline def argsString(x1 :String, x2 :String) :String = "(" + x1 + "," + x2 + ")"
	@inline def argsString(x1 :String, x2 :String, x3 :String) :String = "(" + x1 + "," + x2 + "," + x3 + ")"



	/** Uses java reflection to determine the type name of the `n`-th type argument given to this trait by the concrete
	  * class of this object.
	  */
	def typeArgumentName(fun :Class[_], n :Int, givenTo :Class[_]) :String =
		reflect.typeArgumentOf(fun, n, givenTo) match {
			case null => "_"
			case cls if cls == classOf[AnyRef] => "_"
			case cls :Class[_] => prettyprint.abbrevNameOf(cls)
			case _ => "_,"
		}
}

