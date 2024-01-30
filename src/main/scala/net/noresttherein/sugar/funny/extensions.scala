package net.noresttherein.sugar.funny

import scala.Specializable.Arg
import scala.annotation.tailrec

import net.noresttherein.sugar.extensions.{cast2TypeParamsMethods, castingMethods, downcast2TypeParamsMethods, downcastTypeParamMethods}
import net.noresttherein.sugar.funny.extensions.{Function2Extension, Function3Extension, FunctionExtension, homoFunctionExtension, homoOptFunctionExtension, homoOptionFunctionExtension, HomoPartialFunctionExtension, homoUnsureFunctionExtension, homoMaybeFunctionExtension, PartialFunctionCompanionExtension, PartialFunctionExtension}
import net.noresttherein.sugar.funny.fun.{ComposableFun2, ComposableFun3}
import net.noresttherein.sugar.vars.{Maybe, Opt, Sure, Unsure}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Opt.One




trait extensions extends Any {

	@inline implicit final def FunctionExtension[X, Y](f :X => Y) :FunctionExtension[X, Y] =
		new FunctionExtension(f)

	@inline implicit final def homoFunctionExtension[X](f :X => X) :homoFunctionExtension[X] =
		new homoFunctionExtension(f)

	@inline implicit final def homoOptionFunctionExtension[X](f :X => Option[X]) :homoOptionFunctionExtension[X] =
		new homoOptionFunctionExtension(f)

	@inline implicit final def homoOptFunctionExtension[X](f :X => Opt[X]) :homoOptFunctionExtension[X] =
		new homoOptFunctionExtension(f)

	@inline implicit final def homoMaybeFunctionExtension[X](f :X => Maybe[X]) :homoMaybeFunctionExtension[X] =
		new homoMaybeFunctionExtension(f)

	@inline implicit final def homoUnsureFunctionExtension[X](f :X => Unsure[X]) :homoUnsureFunctionExtension[X] =
		new homoUnsureFunctionExtension(f)

	@inline implicit final def PartialFunctionExtension[X, Y](f :PartialFunction[X, Y]) :PartialFunctionExtension[X, Y] =
		new PartialFunctionExtension(f)

	@inline implicit final def homoPartialFunctionExtension[X](f :PartialFunction[X, X]) :HomoPartialFunctionExtension[X] =
		new HomoPartialFunctionExtension(f)

	@inline implicit final def Function2Extension[A, B, Y](f :(A, B) => Y) :Function2Extension[A, B, Y] =
		new Function2Extension(f)

	@inline implicit final def Function3Extension[A, B, C, Y](f :(A, B, C) => Y) :Function3Extension[A, B, C, Y] =
		new Function3Extension(f)

	@inline implicit final def PartialFunctionCompanionExtension(pf :PartialFunction.type) :PartialFunctionCompanionExtension =
		new PartialFunctionCompanionExtension {}

//	@inline implicit final def partialFunctionFactoryExtension(self :PartialFunction.type)
//			:PartialFunctionFactoryExtension =
//		new PartialFunctionFactoryExtension {}
}




@SerialVersionUID(Ver)
object extensions extends extensions {

	class FunctionExtension[X, Y] private[funny] (private val self :X => Y) extends AnyVal {
		/** Adapts this function to a `PartialFunction` which always returns `true` from `isDefinedAt`. */
		def toPartialFunction :PartialFunction[X, Y] = new FunctionAsPartialFunction(self)
	}

	class homoFunctionExtension[X] private[funny](private val self :X => X) extends AnyVal {
		/** Applies this function to `x` for as long as `pred` is satisfied.
		  * @return `if (pred(x)) x else applyWhile(this(x))(pred)`.
		  */
		@tailrec final def applyWhile(x :X)(pred :X => Boolean) :X =
			if (pred(x)) applyWhile(self(x))(pred)
			else x

		/** Applies this function recursively `n` times to its own return values.
		  * @return `if (n <= 0) x else reapply(this(x), n - 1)`.
		  */
		@tailrec final def reapply(x :X, n :Int) :X = if (n <= 0) x else reapply(self(x), n - 1)
	}

	class homoOptionFunctionExtension[X] private[funny](private val self :X => Option[X]) extends AnyVal {
		/** Applies this function to the argument, and then recursively to its own result, for as long as its result
		  * is non empty. Note that the function must be applied one more time in order to determine the result is empty.
		  * @return `this(x).map(recurse).getOrElse(x)`
		  */
		@tailrec final def reapply(x :X) :X = self(x) match {
			case Some(next) => reapply(next)
			case _ => x
		}
	}

	class homoOptFunctionExtension[X] private[funny](private val self :X => Opt[X]) extends AnyVal {
		/** Applies this function to the argument, and then recursively to its own result, for as long as its result
		  * is non empty. Note that the function must be applied one more time in order to determine the result is empty.
		  * @return `this(x).map(reapply).getOrElse(x)`
		  */
		@tailrec final def reapply(x :X) :X = self(x) match {
			case One(next) => reapply(next)
			case _ => x
		}
	}

	class homoMaybeFunctionExtension[X] private[funny](private val self :X => Maybe[X]) extends AnyVal {
		/** Applies this function to the argument, and then recursively to its own result, for as long as its result
		  * is non empty. Note that the function must be applied one more time in order to determine the result is empty.
		  * @return `this(x).map(reapply).getOrElse(x)`
		  */
		@tailrec final def reapply(x :X) :X = self(x) match {
			case Yes(next) => reapply(next)
			case _ => x
		}
	}

	class homoUnsureFunctionExtension[X] private[funny](private val self :X => Unsure[X]) extends AnyVal {
		/** Applies this function to the argument, and then recursively to its own result, for as long as its result
		  * is non empty. Note that the function must be applied one more time in order to determine the result is empty.
		  * @return `this(x).map(reapply).getOrElse(x)`
		  */
		@tailrec final def reapply(x :X) :X = self(x) match {
			case Sure(next) => reapply(next)
			case _ => x
		}
	}


	class Function2Extension[A, B, Y](private val self :(A, B) => Y) extends AnyVal {
		def andThen[Z](f :Y => Z) :(A, B) => Z = self match {
			case g :ComposableFun2[A, B, Y] => g andThen f
			case _                          => new ComposedFun2(self, f)
		}
	}

	class Function3Extension[A, B, C, Y](private val self :(A, B, C) => Y) extends AnyVal {
		def andThen[Z](f :Y => Z) :(A, B, C) => Z = self match {
			case g :ComposableFun3[A, B, C, Y] => g andThen f
			case _                             => new ComposedFun3(self, f)
		}
	}

	class PartialFunctionExtension[X, Y] private[funny] (private val self :PartialFunction[X, Y]) extends AnyVal {
		/** Same as `PartialFunction.`[[PartialFunction.unapply unapply]], but does not box. */
		def get(x :X) :Maybe[Y] = {
			val y = self.applyOrElse(x, Fallback.downcastParams[X, Y])
			if (y.asAnyRef eq Fallback) No else Yes(y)
		}

		/** Equivalent to `(this andThen f).applyOrElse(x, default)`, but without constructing an intermediate function. */
		def applyAndThenOrElse[O](x :X, f :Y => O, default :X => O) :O = {
			val y = self.applyOrElse(x, Fallback.downcastParams[X, Y])
			if (y.asAnyRef eq Fallback) default(x)
			else f(y)
		}

		/** Equivalent to `(this andThen f).applyOrElse(x, default)`, but without constructing an intermediate function. */
		def applyAndThenOrElse[O](x :X, f :PartialFunction[Y, O], default :X => O) :O = {
			val y = self.applyOrElse(x, Fallback.downcastParams[X, Y])
			if (y.asAnyRef eq Fallback) default(x)
			else f(y)
		}
	}

	class HomoPartialFunctionExtension[X] private[funny] (private val self :PartialFunction[X, X]) extends AnyVal {
		/** Applies this function to the argument, and then recursively to its own result, for as long as it is defined.
		  * @return `this(x).applyOrElse(x, reapply)`
		  */
		@tailrec final def reapply(x :X) :X = {
			val res = self.applyOrElse(x, Fallback.downcastParams[X, X])
			if (res.asAnyRef eq Fallback) x else reapply(res)
		}
	}



	/** Extension factory method `applyOrElse` for `PartialFunction` object. */
	sealed trait PartialFunctionCompanionExtension extends Any {
		@inline def identity[X] :PartialFunction[X, X] = Identity.downcastParam[X]

		/** Forces a function literal of type `(In, In => Out) => Out` to become an instance
		  * of [[PartialFunction]] using it as its `applyOrElse` method, to which other methods
		  * delegate.
		  */
		@inline final def apply[In, Out](applyOrElse :ApplyOrElse[In, Out]) :PartialFunction[In, Out] =
			applyOrElse

		/** Creates a [[PartialFunction]] based on its [[PartialFunction.applyOrElse applyOrElse]] implementation. */
		final def applyOrElse[X, Y](f :(X, X => Y) => Y) :PartialFunction[X, Y] =
			new PartialFunction[X, Y] {
				override def isDefinedAt(x :X) :Boolean =
					f(x, Fallback.downcastParams[X, Y]).asAnyRef ne Fallback

				override def apply(x :X) :Y = {
					val res = f(x, Fallback.downcastParams[X, Y])
					if (res.asAnyRef eq Fallback)
						throw new MatchError(x)
					res
				}

				override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) =
					f(x, default.castParams[X, Y])
			}


		/** Turn a given function returning an `Opt[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @tparam In  the argument type.
		  * @tparam Out the extracted result type.
		  * @param name a textual identifier used in `toString` method of the created `MatchFunction`.
		  * @param f    a function extracting `Out` values from `In` arguments.
		  * @return a partial function extractor wrapping the given function `f`.
		  */
		def from[@specialized(Arg) In, Out]
		        (name :String)(f :In => Opt[Out]) :PartialFunction[In, Out] =
			new OptFunction(f, name)

		/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
		  * that can be used in pattern matching or as a partial function.
		  * @tparam In  the argument type.
		  * @tparam Out the extracted result type.
		  * @param f    a function extracting `Out` values from `In` arguments.
		  * @return a partial function extractor wrapping the given function `f`.
		  */
		def from[@specialized(Arg) In, Out](f :In => Opt[Out]) :PartialFunction[In, Out] =
			new OptFunction(f, "<unlifted function>")
	}


	private[extensions] trait ComposablePartialFunction[-In, +Out] extends PartialFunction[In, Out] {
		override def orElse[A1 <: In, B1 >: Out](that :PartialFunction[A1, B1]) :PartialFunction[A1, B1] =
			PartialFunction[A1, B1] {
				(x :A1, default :A1 => B1) =>
					val out = applyOrElse(x, Fallback.downcastParams[In, Out])
					if (out.asAnyRef ne Fallback) out else that.applyOrElse(x, default)
			}
		override def andThen[C](k :PartialFunction[Out, C]) :PartialFunction[In, C] =
			if (k eq Identity)
				k compose this
			else
				PartialFunction[In, C] {
					(x :In, default :In => C) =>
						val out = applyOrElse(x, Fallback.downcastParams[In, Out])
						if (out.asAnyRef eq Fallback)
							default(x)
						else
							k(out)
				}
		override def compose[R](k :PartialFunction[R, In]) :PartialFunction[R, Out] =
			if (k eq Identity)
				k andThen this
			else
				PartialFunction[R, Out] {
					(x :R, default :R => Out) =>
						val out = applyOrElse(k(x), Fallback.downcastParams[In, Out])
						if (out.asAnyRef ne Fallback) out else default(x)
				}
	}

	/** A ''single abstract method'' subtype of [[PartialFunction]]
	  * requiring from subclasses an implementation of [[PartialFunction.applyOrElse applyOrElse]] method
	  * (as abstract [[net.noresttherein.sugar.funny.extensions.ApplyOrElse.getOrElse getOrElse]]
	  * method). Used primarily in conjunction with
	  * `PartialFunction.`[[net.noresttherein.sugar.funny.extensions.PartialFunctionCompanionExtension.apply apply]]
	  * to create instances of `PartialFunction` which do not resort to intermediate boxing to `Option` in that method.
	  */
	trait ApplyOrElse[In, Out] extends PartialFunction[In, Out] with ComposablePartialFunction[In, Out] {
		def getOrElse(x :In, default :In => Out) :Out

		final override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			getOrElse(x, default.asInstanceOf[In => Out])

		final override def apply(v1 :In) :Out = {
			val z = getOrElse(v1, Fallback.downcastParams[In, Out])
			if (z.asAnyRef eq Fallback)
				throw new MatchError(z)
			z
		}

		final override def isDefinedAt(x :In) :Boolean = {
			val z = getOrElse(x, Fallback.downcastParams[In, Out])
			z.asAnyRef ne Fallback
		}
	}

	private[this] object Fallback extends (Any => Any) {
		override def apply(v1 :Any) :Any = Fallback
	}
	private[this] object Identity extends IdentityPartialFunction[Any]

	private class IdentityPartialFunction[X] extends PartialFunction[X, X] {
		override def isDefinedAt(x :X) :Boolean = true
		override def apply(v1 :X) :X = v1
		override def applyOrElse[A1 <: X, B1 >: X](x :A1, default :A1 => B1) :B1 = x

		override def orElse[A1 <: X, B1 >: X](that :PartialFunction[A1, B1]) = this
//		override def andThen[C](k :X => C) = k match {
//			case partial :PartialFunction[X, C] => partial
//			case _ => super.andThen(k)
//		}
		override def andThen[C](k :PartialFunction[X, C]) = k
		override def compose[R](k :PartialFunction[R, X]) = k
//		override def compose[A](g :A => X) = super.compose(g)

		override def toString = "identity"
	}

	private class FunctionAsPartialFunction[X, Y](f :X => Y) extends ComposablePartialFunction[X, Y] {
		override def isDefinedAt(x :X) :Boolean = true
		override def apply(v1 :X) :Y = f(v1)
		override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) :B1 = f(x)

		override def orElse[A1 <: X, B1 >: Y](that :PartialFunction[A1, B1]) :PartialFunction[A1, B1] = this

		override def andThen[C](k :PartialFunction[Y, C]) :PartialFunction[X, C] =
			if (k eq Identity)
				k compose this
			else
				PartialFunction[X, C] {
					(x :X, default :X => C) =>
						val c = k.applyOrElse(f(x), Fallback.downcastParams[Y, C])
						if (c.asAnyRef ne Fallback) c else default(x)
				}

		override def compose[R](k :PartialFunction[R, X]) :PartialFunction[R, Y] =
			if (k eq Identity)
				k andThen this
			else
				PartialFunction[R, Y] { (arg :R, default :R => Y) =>
					val x = k.applyOrElse(arg, Fallback.downcastParams[R, X])
					if (x.asAnyRef eq Fallback) default(arg) else f(x)
				}

		override def toString = f.toString
	}

	@SerialVersionUID(Ver)
	private final class OptFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	                               (f :In => Opt[Out], override val toString :String)
		extends ComposablePartialFunction[In, Out]
	{
		override def isDefinedAt(x :In) :Boolean = f(x).isDefined

		override def apply(v1 :In) :Out = f(v1) getOrElse { throw new MatchError(v1) }

		override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			f(x) getOrElse default(x)
	}

}
