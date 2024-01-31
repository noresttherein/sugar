package net.noresttherein.sugar.vars

import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.Ref.undefined




/** A lazy ''by-name'' expression which may, but needs not to, evaluate to a value of type `T`.
  * It is functionally equivalent to [[net.noresttherein.sugar.vars.Lazy Lazy]]`[Maybe[T]]`.
  * @define Ref `LazyUnsure`
  * @define ref unsure lazy value
  * @define coll unsure lazy value
  * @author Marcin Mościcki
  */
trait LazyUnsure[@specialized(SpecializedVars) +T] extends Ref[T] {
	/** True, if the expression has been evaluated and returned value. */
	@inline final override def nonEmpty :Boolean = isDefinite

	/** Returns `!`[[net.noresttherein.sugar.vars.LazyUnsure.isDefinite isDefinite]]. */
	override def isEmpty :Boolean = !isDefinite //overridden for docs only

	/** Returns [[net.noresttherein.sugar.vars.Val.isDefinite isDefinite]]. */
	final override def isFinalizable :Boolean = isDefinite

	/** Returns [[net.noresttherein.sugar.vars.Val.isDefinite isDefinite]]. */
	@inline final override def isConst :Boolean = isDefinite

	/** Checks if the lazy expression evaluates to a concrete value.
	  * This will evaluate the expression, if it has not been evaluated previously!
	  * @see [[net.noresttherein.sugar.vars.Val.isDefinite isDefinite]].
	  */
	@inline override def isDefined :Boolean = isDefinite

	/** Checks if the lazy expression was evaluated to a concrete value.
	  * It will not attempt to evaluate the value itself, returning `false` instead. It will return `false`
	  * even if the initializer has been evaluated, but did not produce a value.
	  * Use [[net.noresttherein.sugar.vars.LazyUnsure.isFinal isFinal]] to check exclusively
	  * for the initialization state.
	  * If `true`, then [[net.noresttherein.sugar.vars.LazyUnsure.maybe opt]]`.get` will not throw an exception.
	  */
	override def isDefinite :Boolean = maybe.nonEmpty

	/** The evaluated value of this $Ref.
	  * @return `this.`[[net.noresttherein.sugar.vars.LazyUnsure.get get]].
	  */
	@inline final override def apply() :T = get

	/** The value of this $Ref, if it has been already evaluated, and is non empty.
	  * Throws a [[NoSuchElementException]] otherwise.
	  */
	override def value :T = maybe.get

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.LazyUnsure.const const]].
	  */
	override def get :T = toMaybe.get

	/** The value of this $Ref. This method always returns the same value and throws no exception,
	  * but may block in order to avoid initialization. Same as [[net.noresttherein.sugar.vars.LazyUnsure.get get]].
	  */
	@inline final override def const :T = get

	/** Returns [[net.noresttherein.sugar.vars.Maybe.Yes Yes]]`(`[[net.noresttherein.sugar.vars.Lazy.get get]]`)`,
	  * same as [[net.noresttherein.sugar.vars.Lazy.toMaybe toMaybe]].
	  */
	@inline final override def maybeConst :Maybe[T] = maybe

	/** Creates a new $Ref`[O]` instance with the same characteristics as this instance, evaluated
	  * to the application of `f` to the value of this instance. If the value has already been evaluated,
	  * the created instance will be eagerly evaluated - use $Ref`(f(this.value))` if you wish for `f`
	  * to not be executed before the method returns under any circumstances. If this instance initializes
	  * under a `synchronized` block, it is guaranteed that it will be evaluated at most once, regardless of
	  * which of the two values are accessed. Created $Ref`[O]` will likewise use the `synchronized` block
	  * in that case and `f` will be evaluated at most once.
	  */
	def map[O](f :T => O) :LazyUnsure[O]

	/** Creates a new $Ref`[O]` initialized with the expression `f(this.value)).value`. If this instance is already
	  * evaluated, the function will be applied immediately and its result returned directly. Otherwise a new
	  * $Ref`[O]` with the same synchronization characteristics as this instance will be created, with
	  * `f(this.value)).value` as the initializing expression. If you wish for `f` to not be executed
	  * before the method returns and the returned instance is accessed, use $Ref`(f(this.value).value))`.
	  */
	def flatMap[O](f :T => LazyUnsure[O]) :LazyUnsure[O]

	override def mkString :String = mkString("LazyUnsure")
}




@SerialVersionUID(Ver)
object LazyUnsure {
	def apply[@specialized(SpecializedVars) T](init: => Maybe[T]) :LazyUnsure[T] = new SyncUnsure(() => init)

	def eager[@specialized(SpecializedVars) T](value :T) :LazyUnsure[T] = new EagerUnsure(Yes(value))

	def fromOpt[@specialized(SpecializedVars) T](value :Maybe[T]) :LazyUnsure[T] =
		if (value.isEmpty) Empty else new EagerUnsure(value)

	val Empty :LazyUnsure[Nothing] = new EagerUnsure(No)


	@SerialVersionUID(Ver)
	private class EagerUnsure[+T](x :Maybe[T]) extends LazyUnsure[T] {
		override def isFinal = x.isDefined
		override def maybe   = x
		override def toMaybe = x

		override def map[O](f :T => O) = if (x.isDefined) new EagerUnsure(Yes(f(x.get))) else Empty
		override def flatMap[O](f :T => LazyUnsure[O]) =
			if (x.isDefined) f(x.get) else Empty
	}



	@SerialVersionUID(Ver)
	private final class SyncUnsure[+T](private[this] var initializer :() => Maybe[T]) extends LazyUnsure[T] {
		@volatile private[this] var evaluated :Any = undefined

		override def isFinal :Boolean = evaluated != undefined
		override def isDefinite :Boolean = evaluated != undefined && evaluated.asInstanceOf[Maybe[T]].isDefined

		override def value :T = evaluated match {
			case Ref.undefined => noSuch_!("Uninitialized LazyUnsure")
			case v => v.asInstanceOf[Maybe[T]].get
		}
		override def maybe :Maybe[T] = synchronized {
			val res = evaluated
			if (res == undefined) No else res.asInstanceOf[Maybe[T]]
		}
		override def toMaybe :Maybe[T] = {
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
			res.asInstanceOf[Maybe[T]]
		}

		override def map[O](f :T => O) :LazyUnsure[O] = {
			var opt = evaluated
			if (opt != undefined)
				eager(f(opt.asInstanceOf[T]))
			else synchronized {
				opt = evaluated
				if (opt == undefined)
					new SyncUnsure[O](() => toMaybe.map(f))
				else
					fromOpt(opt.asInstanceOf[Maybe[T]].map(f))
			}
		}
		override def flatMap[O](f :T => LazyUnsure[O]) :LazyUnsure[O] = {
			var opt = evaluated
			if (opt != undefined)
				opt.asInstanceOf[Maybe[T]] match {
					case Yes(v) => f(v)
					case _      => Empty
				}
			else synchronized {
				opt = evaluated
				if (opt != undefined)
					opt.asInstanceOf[Maybe[T]] match {
						case Yes(v) => f(v)
						case _      => Empty
					}
				else
					new SyncUnsure(() => toMaybe.flatMap(f(_).maybe))
			}
		}

		override def mkString :String = mkString("SyncUnsure")

		private def writeReplace = LazyUnsure.eager(get)
	}

}
