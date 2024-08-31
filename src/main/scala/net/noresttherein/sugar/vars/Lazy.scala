package net.noresttherein.sugar.vars

import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.{RefFractional, RefIntegral, RefNumeric}




/** An implementation of a lazy value which preserves the laziness property in its
  * [[net.noresttherein.sugar.vars.Lazy.map map]]
  * and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]] methods.
  * @define Ref `Delayed`
  * @define ref delayed value
  * @author Marcin Mościcki
  */
trait Lazy[@specialized(SpecializedVars) +T] extends Delayed[T] {

	/** Creates a new $Ref`[O]` instance evaluating to the application of `f` to the value of this instance.
	  * The function is not applied until the moment when the value of the returned instance is accessed.
	  * If the value has already been evaluated, created instance will not depend on this $ref.
	  */
	override def map[O](f :T => O) :Lazy[O] =
		new MappedVal[T, O](this, f) with Lazy[O]

	/** Creates a new $Ref`[O]` of value equal to `f(this.value).value`.
	  * The function is not applied until the moment when the value of the returned instance is accessed.
	  * If the value has already been evaluated, the created $ref will not depend on this instance.
	  */
	override def flatMap[O](f :T => Delayed[O]) :Lazy[O] =
		new FlatMappedVal[T, O](this, f) with Lazy[O]

	override def mkString :String = mkString("Lazy")
}



@SerialVersionUID(Ver)
case object Lazy {

	/** Creates a wrapper over a lazily initialized value roughly equivalent to a built-in `lazy val`.
	  * The differences are:
	  *   - no synchronization penalty for value types, once the value has been initialized;
	  *   - monadic [[net.noresttherein.sugar.vars.Lazy.map map]]
	  *     and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]] operations;
	  *   - the initializer expression is freed for garbage collection;
	  *   - [[net.noresttherein.sugar.vars.Delayed.isEmpty isEmpty]] test method.
	  *     The implementation returned by this method uses its synchronized lock to ensure the `init` block
	  *     is called at most once.
	  */
	@inline def apply[@specialized(SpecializedVars) T](init : => T) :Lazy[T] = from(() => init)

	/** Same as [[net.noresttherein.sugar.vars.Lazy.apply apply]], but accepts a `Function0`,
	  * rather a by-name parameter.
	  */
	def from[@specialized(SpecializedVars) T](init: () => T) :Lazy[T] =
		new Val[T](init) match {
			case ref if ref.getClass == classOf[Val[Any]] => new Ref(init)
			case spec => spec
		}

	private def eager[@specialized(SpecializedVars) T](value :T) :Lazy[T] = new Eager(value)

	implicit def delayedNumeric[T :Numeric] :Numeric[Lazy[T]] = new RefNumeric[Lazy, T] with DelayedTypeClass[T]
	implicit def delayedIntegral[T :Integral] :Integral[Lazy[T]] =
		new RefIntegral[Lazy, T] with DelayedTypeClass[T]

	implicit def delayedFractional[T :Fractional] :Fractional[Lazy[T]] =
		new RefFractional[Lazy, T] with DelayedTypeClass[T]

	private trait DelayedTypeClass[T] extends RefNumeric[Lazy, T] {
		override def fromInt(x :Int) :Lazy[T] = Lazy(underlying.fromInt(x))
		protected override def fmap(x :Lazy[T], y :Lazy[T])(op :(T, T) => T) :Lazy[T] =
			for (xv <- x; yv <- y) yield op(xv, yv)

		protected override def map(x :Lazy[T])(f :T => T) :Lazy[T] = x.map(f)
		protected override def apply(x :T) :Lazy[T] = Lazy.eager(x)
	}



	private trait Impl[@specialized(SpecializedVars) +T] extends Lazy[T] {
		override def map[O](f :T => O) :Lazy[O] = opt match {
			case One(v) => Lazy.from(() => f(v))
			case _      => Lazy.from(() => f(get))
		}
		//A scalac error occurs if we remove @unspecialized
		@unspecialized override def flatMap[O](f :T => Delayed[O]) :Lazy[O] = opt match {
			case One(v) => f(v) match {
				case other :Lazy[O] => other
				case lzy =>
					(lzy.opt :Opt[O]) match {
						case One(o) => Lazy.eager(o)
						case _      => Lazy.from(lzy)
					}
			}
			case _ => Lazy.from(() => f(get).get)
		}
	}

	//todo: specialization with caching through manual specialization.
	@SerialVersionUID(Ver)
	private class Val[@specialized(SpecializedVars) +T](initializer: () => T) extends DelayedVal[T] with Impl[T] {
		init(initializer)
	}

	@SerialVersionUID(Ver)
	private class Ref[+T](initializer: () => T) extends SyncDelayedRef[T](initializer) with Impl[T]

	@SerialVersionUID(Ver) //todo: make it specialized
	//Not specialized to avoid boxing of T to ref-wrapper-of-T, especially that we likely already have the wrapper
	private final class Eager[+T](override val get :T) extends Const[T] with Lazy[T] {
		override def map[O](f :T => O) :Lazy[O] = Lazy.from(() => f(get))
		override def flatMap[O](f :T => Delayed[O]) :Lazy[O] = f(get) match {
			case other :Lazy[O] => other
			case lzy => Lazy.from(lzy)
		}
//		override def flatMap[O](f :T => Delayed[O]) :Delayed[O] = f(get)
		override def isSpecialized :Boolean = false //getClass == classOf[Eager[_]]
	}
}
