package net.noresttherein.sugar.vars

import net.noresttherein.sugar.exceptions.noSuch_!
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One
import net.noresttherein.sugar.vars.Ref.undefined




/** A lazy ''by-name'' expression which may, but needs not to, evaluate to a value of type `T`.
  * It is functionally equivalent to [[net.noresttherein.sugar.vars.Delayed Delayed]]`[Maybe[T]]`.
  * @author Marcin Mościcki
  * @define Ref `UnsureDelayed`
  * @define ref unsure lazy value
  * @define coll unsure lazy value
  */
trait UnsureDelayed[@specialized(SpecializedVars) +T] extends Ref[T] {
	/** True, if the expression has been evaluated and returned value. */
	@inline final override def nonEmpty :Boolean = isDefinite

	/** Returns `!`[[net.noresttherein.sugar.vars.UnsureDelayed.isDefinite isDefinite]]. */
	override def isEmpty :Boolean = !isDefinite //overridden for docs only

	/** Returns [[net.noresttherein.sugar.vars.Ref.isDefinite isDefinite]]. */
	final override def isFinalizable :Boolean = isDefinite

	/** Returns [[net.noresttherein.sugar.vars.Ref.isDefinite isDefinite]]. */
	@inline final override def isConst :Boolean = isDefinite

	/** Checks if the lazy expression evaluates to a concrete value.
	  * This will evaluate the expression, if it has not been evaluated previously!
	  * @see [[net.noresttherein.sugar.vars.Ref.isDefinite isDefinite]].
	  */
	@inline override def isDefined :Boolean = toMaybe.isDefined

	/** Checks if the lazy expression was evaluated to a concrete value.
	  * It will not attempt to evaluate the value itself, returning `false` instead. It will return `false`
	  * even if the initializer has been evaluated, but did not produce a value.
	  * Use [[net.noresttherein.sugar.vars.UnsureDelayed.isFinal isFinal]] to check exclusively
	  * for the initialization state.
	  * If `true`, then [[net.noresttherein.sugar.vars.UnsureDelayed.maybe opt]]`.get` will not throw an exception.
	  */
	override def isDefinite :Boolean = maybe.nonEmpty

	/** The evaluated value of this $Ref.
	  * @return `this.`[[net.noresttherein.sugar.vars.UnsureDelayed.get get]].
	  */
	@inline final override def apply() :T = get

	/** The value of this $Ref, if it has been already evaluated, and is non-empty.
	  * Throws a [[NoSuchElementException]] otherwise.
	  */
	override def value :T = opt.get

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.UnsureDelayed.const const]].
	  */
	override def get :T = toOpt.get

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.UnsureDelayed.get get]].
	  */
	@inline final override def const :T = get

	/** Evaluates the expression and returns the yielded value. */
	@inline final override def maybeConst :Maybe[T] = toMaybe

	/** Returns [[net.noresttherein.sugar.vars.Opt.One One]]`(`[[net.noresttherein.sugar.vars.Delayed.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Delayed.toOpt toOpt]].
	  */
	@inline final override def constOpt :Opt[T] = toOpt

	/** Creates a new $Ref`[O]` instance with the same characteristics as this instance, evaluated
	  * to the application of `f` to the value of this instance. If the value has already been evaluated,
	  * the created instance will be eagerly evaluated - use $Ref`(f(this.value))` if you wish for `f`
	  * to not be executed before the method returns under any circumstances. If this instance initializes
	  * under a `synchronized` block, it is guaranteed that it will be evaluated at most once, regardless of
	  * which of the two values are accessed. Created $Ref`[O]` will likewise use the `synchronized` block
	  * in that case and `f` will be evaluated at most once.
	  */
	def map[O](f :T => O) :UnsureDelayed[O]

	/** Creates a new $Ref`[O]` initialized with the expression `f(this.value).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise, a new
	  * $Ref`[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use $Ref`(f(this.value).value)`.
	  */
	def flatMap[O](f :T => UnsureDelayed[O]) :UnsureDelayed[O]

	override def mkString :String = mkString("LazyUnsure")
}




@SerialVersionUID(Ver)
case object UnsureDelayed {
	def apply[@specialized(SpecializedVars) T](init: => Opt[T]) :UnsureDelayed[T] = new SyncUnsureDelayed(() => init)

	def eager[@specialized(SpecializedVars) T](value :T) :UnsureDelayed[T] = new EagerUnsureDelayed(One(value))

	def fromOpt[@specialized(SpecializedVars) T](value :Opt[T]) :UnsureDelayed[T] =
		if (value.isEmpty) empty else new EagerUnsureDelayed(value)

	val empty :UnsureDelayed[Nothing] = new EagerUnsureDelayed(None)


	@SerialVersionUID(Ver)
	private class EagerUnsureDelayed[+T](x :Opt[T]) extends UnsureDelayed[T] {
		override def isFinal = x.isDefined
		override def opt   = x
		override def toOpt = x

		override def map[O](f :T => O) = if (x.isDefined) new EagerUnsureDelayed(One(f(x.get))) else empty
		override def flatMap[O](f :T => UnsureDelayed[O]) =
			if (x.isDefined) f(x.get) else empty
	}



	@SerialVersionUID(Ver)
	private final class SyncUnsureDelayed[+T](private[this] var initializer :() => Opt[T]) extends UnsureDelayed[T] {
		@volatile private[this] var evaluated :Any = undefined

		override def isFinal :Boolean = evaluated != undefined
		override def isDefinite :Boolean = evaluated != undefined && evaluated.asInstanceOf[Opt[T]].isDefined

		override def value :T = evaluated match {
			case Ref.undefined => noSuch_!("Uninitialized LazyUnsure")
			case v => v.asInstanceOf[Opt[T]].get
		}
		override def opt :Opt[T] = synchronized {
			val res = evaluated
			if (res == undefined) None else One(res.asInstanceOf[T])
		}
		override def toOpt :Opt[T] = {
			var res = evaluated
			if (res == undefined)
				synchronized {
					res = evaluated
					if (res == undefined) {
						res = initializer()
						evaluated = res
						initializer = null
					}
				}
			res.asInstanceOf[Opt[T]]
		}

		override def map[O](f :T => O) :UnsureDelayed[O] = {
			var opt = evaluated
			if (opt != undefined)
				eager(f(opt.asInstanceOf[T]))
			else synchronized {
				opt = evaluated
				if (opt == undefined)
					new SyncUnsureDelayed[O](() => toOpt.map(f))
				else
					fromOpt(opt.asInstanceOf[Opt[T]].map(f))
			}
		}
		override def flatMap[O](f :T => UnsureDelayed[O]) :UnsureDelayed[O] = {
			var opt = evaluated
			if (opt != undefined)
				opt.asInstanceOf[Opt[T]] match {
					case One(v) => f(v)
					case _      => empty
				}
			else synchronized {
				opt = evaluated
				if (opt != undefined)
					opt.asInstanceOf[Opt[T]] match {
						case One(v) => f(v)
						case _      => empty
					}
				else
					new SyncUnsureDelayed(() => toOpt.flatMap(f(_).opt))
			}
		}

		override def mkString :String = mkString("SyncUnsure")

		private def writeReplace :AnyRef = UnsureDelayed.eager(get)
	}

}
