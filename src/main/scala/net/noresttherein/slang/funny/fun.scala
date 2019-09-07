package net.noresttherein.slang.funny

import scala.annotation.unspecialized
import scala.reflect.ClassTag


/** Factory methods for functions with benefits.
  * @author Marcin Mościcki marcin@moscicki.net
  */
object fun {
	import specializations._


	/** Creates a named function. The string given as the first argument is used in `toString`
	  * of the given function, replacing the rather unhelpful '&lt;function1&gt;'. The second argument is the returned
	  * function itself; however, being a ''SAM'' type, a function expression is automatically converted to the target
	  * function type. Hence, the syntax would be:
	  * {{{
	  *     fun("square") { x :Int => x * x }
	  * }}}
	  * This method simply sets the name on the second argument to the given string and returns it as the standard
	  * function type `X => Y` for better type inference.
	  * @param name name to use as the textual representation of the given function
	  * @param f renamed function
	  * @return `f`
	  */
	def apply[X, Y](name :String)(f :NamedFun[X, Y]) :X => Y = {
		f.rename(name); f
	}


	/** Wraps the given function in a decorator returning the given name from its `toString` method.
	  * @see [[net.noresttherein.slang.funny.fun.apply]]
	  */
	def name[@specialized(ArgTypes) X, @specialized(ReturnTypes) Y](name :String)(f :X => Y) :X => Y =
		new ComposableFun[X, Y] {
			override def apply(x :X) :Y = f(x)
			override def toString = name
		}


	/** Equivalent to `scala.identity[X]`, but is specialized and overrides `compose` (and `andThen`)
	  * for reduced overhead of function composition. Additionally, it provides a more informative `toString` output.
	  */
	def ident[@specialized(ArgTypes) X] :X=>X = new Identity[X]{}


	/** A constant function equivalent to `(_:X) => value`, but with a more helpful `toString` and performing reduction on composition.
	  * For any `g :Y=>O`, `f endThen g` eagerly evaluates to `fun[X, O](g(value))`. Additionally, for any `h :A=>X`
	  * implementing [[net.noresttherein.slang.funny.fun.PureFun PureFun]], `f compose h` evaluates simply
	  * to `fun[A, Y](value)`.
	  */
	def const[@specialized(ArgTypes) X, @specialized(ReturnVals) Y](result :Y) :X => Y = new ConstFun(result)


	/** A function throwing the exception returned for its argument by the passed constructor function.
	  * Provides an informative textual representation and performs reduction during composition, ignoring any
	  * function passed to its `andThen`, returning itself instead.
	  */
	def throwing[X](exception :X => Throwable) :X => Nothing = new ThrowingFunction(exception)


	/** A function which always throws the given exception. Provides informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.slang.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	def throwing(exception :Throwable) :Any => Nothing = new Throw(exception)


	/** A function which always throws the exception provided as the type parameter. The `T` must be a concrete class
	  * implementing throwable and providing a default no-argument constructor or this method will throw an
	  * `InstantiationException`. Returned function provides an informative `toString` implementation and absorbs
	  * any function applied to its result during composition by its `andThen` method. Additionally, any function,
	  * passed to its `compose` method, which implements [[net.noresttherein.slang.funny.fun.PureFun PureFun]],
	  * will likewise be reduced during composition to a directly throwing function.
	  */
	def throwing[T <: Throwable :ClassTag] :Any => Nothing =
		new Throw(implicitly[ClassTag[T]].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable])






	/** Declaration of type groups usable as arguments to scala `@specialized` annotation. */
	object specializations {
		final val ReturnVals = new Specializable.Group(Boolean, Int, Float, Long, Double)

		/** Types for which `Function1`'s result type is specialized. */
		final val ReturnTypes = new Specializable.Group(Unit, Boolean, Int, Float, Long, Double)
		/** Types for which `Function1`'s argument type is specialized. */
		final val ArgTypes = new Specializable.Group(Int, Long, Float, Double)



		private[fun] class SpecializedType[@specialized(ReturnTypes) T] {

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
				else "?"
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


	/** Marker trait for functions which are not expected to throw any exceptions. */
	trait PureFun[-X, +Y] extends (X => Y)



	trait ComposableFun[@specialized(ArgTypes) -X, @specialized(ReturnVals) +Y] extends (X=>Y) {

		override def compose[A](g: A => X): A => Y = g match {
			case f :ComposableFun[A, X] => f chainBefore this
			case _ => new ComposedFun(g, this)
		}

		override def andThen[A](g: Y => A): X => A = g match {
			case f :ComposableFun[Y, A] => f chainAfter this
			case _ => new ComposedFun(this, g)
		}

		protected[fun] def chainBefore[@specialized(ReturnVals) Z](g  :Y => Z) :X => Z =
			new ComposedFun(this, g)

		protected[fun] def chainAfter[@specialized(ArgTypes) W](g :W => X) :W => Y =
			new ComposedFun(g, this)

		/** Double dispatch target of `andThen` to ensure proper specialization. */
		private[fun] def chainAfter[@specialized(ArgTypes) W](const :X) :W => Y = new ConstFun[W, Y](apply(const))


		def canEqual(other :Any) :Boolean = false

		private[this] def domain :SpecializedType[X] = new SpecializedType[X]
		private[this] def range :SpecializedType[Y] = new SpecializedType[Y]

		def domainString :String = domain.toString
		def rangeString :String = range.toString

		def typeString :String = domainString + "=>" + rangeString

		override def toString :String = typeString
	}



	private class ComposedFun[@specialized(ArgTypes) -X, @specialized(ArgTypes) Y, @specialized(ReturnTypes) +Z](val inner :X=>Y, val outer :Y=>Z)
		extends ComposableFun[X, Z]
	{
		override def apply(x: X): Z = outer(inner(x))

		@unspecialized
		override def compose[A](g :A => X) :A => Z = outer compose (inner compose g)

		@unspecialized
		override def andThen[A](g :Z => A) :X => A = inner andThen (outer andThen g)


		override def equals(that :Any) :Boolean = that match {
			case c :ComposedFun[_, _, _] => (c eq this) || c.inner==inner && c.outer==outer
			case _ => false
		}

		override def hashCode :Int = inner.hashCode * 31 + outer.hashCode

		override def toString :String =
			(outer, inner) match {
				case (f :ComposableFun[_, _], g :ComposableFun[_, _]) =>
					val innerS = g.toString
					val outerS = f.toString
					val outerTypePrefix = f.domainString + "=>"
					if (innerS.endsWith("=>" + g.rangeString) && outerS.startsWith(outerTypePrefix))
						innerS + outerS.substring(outerTypePrefix.length)
					else
						innerS + "*>" + outerS

				case (f :ComposableFun[_, _], _) if inner.toString == "<function1>" =>
					val outerS = f.toString
					if (outerS.startsWith(f.domainString + "=>"))
						"?=>" + outerS
					else
						"?=>?*>" + outerS
				case (_, g :ComposableFun[_, _]) if outer.toString == "<function1>" =>
					val innerS = g.toString
					if (innerS.endsWith("=>" + g.rangeString))
						innerS + "=>?"
					else
						innerS + "*>?=>?"
				case _ =>
					inner + "*>" + outer
			}

	}



	abstract class NamedFun[@specialized(ArgTypes) -X, @specialized(ReturnTypes) +Y] extends ComposableFun[X, Y] {
		def this(name :String) = { this(); this.name = name }

		@volatile private[this] var name :String = _

		private[fun] def rename(name :String) :Unit = { this.name = name }

		override def toString :String = {
			var res = name
			if (res == null) {
				res = typeString
				name = res
			}
			res
		}

	}


	trait Identity[@specialized(ArgTypes) X] extends ComposableFun[X, X] with PureFun[X, X] {
		final override def apply(x :X) :X = x

		@unspecialized
		override def compose[A](g :A => X) :A => X = g

		@unspecialized
		override def andThen[A](g :X => A) :X => A = g

		@unspecialized
		override protected[fun] def chainBefore[@specialized(ReturnVals) Z](g :X => Z) :X => Z = g

		@unspecialized
		override protected[fun] def chainAfter[@specialized(ArgTypes) W](g :W => X) :W => X = g


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Identity[_]]

		override def equals(that :Any) :Boolean = that match {
			case id :Identity[_] => (id eq this) || canEqual(id) && id.canEqual(this) && {
				val domain = domainString
				domain.length > 1 && domain == id.domainString
			}
			case _ => false
		}

		override def hashCode :Int = domainString.hashCode

		override def toString :String = "Identity[" + domainString + "]"
	}



	private class ConstFun[@specialized(ArgTypes) -X, @specialized(ReturnTypes) +Y](result :Y)
		extends ComposableFun[X, Y] with PureFun[X, Y]
	{
		final override def apply(x :X) :Y = result

		override def compose[A](g :A => X) :A => Y = chainAfter(g)

		override def andThen[A](g :Y => A) :X => A = chainBefore(g)

		override protected[fun] def chainBefore[@specialized(ReturnVals) Z](g :Y => Z) :X => Z =
			try {
				new ConstFun(g(result))
			} catch {
				case _ :Throwable => super.chainBefore(g)
			}

		override protected[fun] def chainAfter[@specialized(ArgTypes) W](g :W => X) :W => Y = g match {
			case _ :ConstFun[_, _] => new ConstFun(result)
			case _ => new ComposedFun(g, this)
		}


		override def toString :String = {
			val x = domainString
			if (domainString == "_")
				"_=>" + result
			else
				"_:"+domainString + "=>" + result
		}

	}



	private class ThrowingFunction[X](exception :X => Throwable) extends ComposableFun[X, Nothing] {
		override def apply(x :X) :Nothing = throw exception(x)

		override def compose[A](g :A => X) :A => Nothing = chainAfter(g)

		override def andThen[A](g :Nothing => A) :X => A = this

		override protected[fun] def chainBefore[@specialized(ReturnTypes) Z](g :Nothing => Z) :X => Z = this

		override protected[fun] def chainAfter[@specialized(ArgTypes) W](g :W => X) :W => Nothing =
			new ThrowingFunction(g andThen exception)


		override def rangeString = "Nothing"
	}



	private class Throw[X](exception :Throwable) extends ComposableFun[X, Nothing] {
		override def apply(x :X) :Nothing = throw exception

		override def compose[A](g :A => X) :A => Nothing = chainAfter(g)

		override def andThen[A](g :Nothing => A) :X => A = this

		override protected[fun] def chainBefore[@specialized(ReturnTypes) Z](g :Nothing => Z) :X => Z = this

		override protected[fun] def chainAfter[@specialized(ArgTypes) W](g :W => X) :W => Nothing =
			if (g.isInstanceOf[PureFun[_, _]]) new Throw[W](exception)
			else new ComposedFun(g, this)

		override def rangeString :String = "Nothing"

		override def toString :String = "throw "+exception
	}




}

