package net.noresttherein.sugar.vars

import java.lang.ref.Cleaner.Cleanable

import net.noresttherein.sugar.unsupported_!
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** `Clearable` is the opposite of [[net.noresttherein.sugar.vars.Out Out]]: a value box initialized on construction,
  * but with its contents being subject to being cleared - unreferenced and freed for a garbage collector.
  * The value itself cannot be changed in other way than removing it.
  * It is not a [[net.noresttherein.sugar.vars.DisposableRef DisposableRef]] as it uses a hard reference
  * and must be cleared manually, rather than by the garbage collector. It is typically used for temporary data,
  * such as initializer blocks, which become no longer needed once the final content is created.
  * While this functionality can be in most cases covered by a simple `var`, a lack of a setter allows this type
  * to be covariant in its content type parameter `T`. Consider a pattern for lazily initialized values:
  * {{{
  *     trait B {
  *         @volatile protected var init :() => B
  *         @volatile private var cached :B = _
  *         def get :B = {
  *             val cons = init
  *             if (cons == null)
  *                 cached
  *              else {
  *                 cached = cons()
  *                 init = null
  *              }
  *              cached
  *         }
  *     }
  *
  *     Class A(value: => A) extends B {
  *         @volatile override var init :() => A = () => value
  *     }
  * }}}
  * Class `A` does not compile, because a `var` of type `B` cannot be overriden by a `var` of type `A`,
  * as property setters are contravariant. In this case we however do not need to assign arbitrary values
  * to the variable, only a `null`. The code above can be rewritten using `Clearable`:
  * {{{
  *     trait B {
  *         protected val init :Clearable[() => B]
  *         @volatile private var cached :B = _
  *         def get :B =
  *             init.option match {
  *                 case Some(cons) =>
  *                     cached = cons(); init.clear()
  *                 case _ => cached
  *             }
  *     }
  *     class A(value: => A) extends B {
  *         protected override val init = Clearable.volatile
  *     }
  * }}}
  *
  * For additional utility, this trait implements also Java [[AutoCloseable]]
  * and [[java.lang.ref.Cleaner.Cleanable Cleanable]].
  *
  * @define Ref `Clearable`
  * @define ref clearable value
  * @author Marcin Mościcki
  */
trait Clearable[+T] extends Ref[T] with AutoCloseable with Cleanable with Serializable {

	/** Returns `true` ''iff'' this `Clearable` is empty. */
	@inline final override def isFinal :Boolean = maybe.isEmpty

	/** Checks if this variable currently holds a value. Note that,
	  * unlike with [[net.noresttherein.sugar.vars.Lazy Lazy]], this property is `true` when the object is created
	  * and, at some point, may become `false` and remains so for the remainder of this object's life.
	  * It makes this method of very dubious utility, as any positive value can be outdated before even
	  * it is returned to the caller. It can however still be used as a flag signaling that some other variable
	  * is initialized, if the initialization of the latter is synchronized with clearing of this object.
	  * In order to access the value, use [[net.noresttherein.sugar.vars.Ref.maybe maybe]],
	  * [[net.noresttherein.sugar.vars.Ref.option option]] or [[net.noresttherein.sugar.vars.Ref.unsure unsure]].
	  */
	override def isEmpty :Boolean = maybe.isEmpty

	/** Returns `false` because a `Clearable` is not (effectively) immutable. */
	@inline final override def isFinalizable: Boolean = false

	/** Returns `false`. */
	final override def isConst: Boolean = false

	/** Returns `!`[[net.noresttherein.sugar.vars.Clearable.isEmpty isEmpty]]. */
	@inline final override def isDefined :Boolean = !isEmpty

	/** Returns `true` ''iff'' this `Clearable` is not empty. */
	@inline final override def isDefinite :Boolean = !isEmpty

	/** Returns `0` if the variable has been [[net.noresttherein.sugar.vars.Clearable.clear cleared]], or `1` otherwise. */
	@inline final def size :Int = maybe.size

	/** Same as [[net.noresttherein.sugar.vars.Clearable.get get]]. */
	@inline final override def value :T = get

	/** Throws an [[UnsupportedOperationException]]. */
	@inline final override def const :Nothing = unsupported_!("Clearable.const")

	@inline final override def apply() :T = get

	/** Same as [[net.noresttherein.sugar.vars.Clearable.option option]]. */
	@inline final override def toOption :Option[T] = option

	/** Returns [[None]]. */
	@inline final override def constOption :Option[T] = None

	/** Same as [[net.noresttherein.sugar.vars.Clearable.maybe opt]]. */
	@inline final override def toMaybe :Maybe[T] = maybe

	/** Returns [[net.noresttherein.sugar.vars.Maybe.No No]]. */
	@inline final override def maybeConst :Maybe[T] = No

	/** Same as [[net.noresttherein.sugar.vars.Clearable.unsure unsure]]. */
	@inline final override def toUnsure :Unsure[T] = unsure

	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]]. */
	@inline final override def unsureConst :Unsure[T] = Missing

	/** Clears this variable, returning its current value. If this instance is already cleared,
	  * an [[NoSuchElementException]] will be thrown.
	  */
	@inline final def remove() :T = { val res = get; clear(); res }

	/** Clears this variable, returning its current value, if any. */
	@inline final def removeOpt() :Maybe[T] = { val res = maybe; clear(); res }

	/** Resets this variable to an undefined state, unreferencing its contents. */
	def clear() :Unit

	/** Resets this variable to an undefined state, unreferencing its contents.
	  * Same as [[net.noresttherein.sugar.vars.Clearable.clear clear]].
	  * This is an [[AutoCloseable]] method.
	  */
	override def close() :Unit = clear()

	/** Resets this variable to an undefined state, unreferencing its contents.
	  * Same as [[net.noresttherein.sugar.vars.Clearable.clear clear]].
	  * This is a [[java.lang.ref.Cleaner.Cleanable Cleanable]] method.
	  */
	override def clean() :Unit = clear()

	override def mkString :String = mkString("Clearable")
	override def toString :String = maybe match {
		case Yes(v) => String.valueOf(v)
		case _      => "<cleared>"
	}
}




@SerialVersionUID(Ver)
object Clearable {
	/** A non synchronized, non thread safe `Clearable` variable initialized with the given value. */
	def apply[T](value :T) :Clearable[T] = new Plain[T](Yes(value))

	/** A `Clearable` variable initialized with the given value, synchronizing all access on its monitor. */
	def sync[T](value :T) :Clearable[T] = new Synced(Yes(value))

	/** A `Clearable` instance backed by a `@volatile` variable. */
	def volatile[T](value :T) :Clearable[T] = new Volatile(value)


	@SerialVersionUID(Ver)
	private class Plain[+T](private[this] var x :Maybe[T]) extends Clearable[T] {
		override def get :T = x.get
		override def maybe :Maybe[T] = x
		override def clear() :Unit = x = No
	}

	@SerialVersionUID(Ver)
	private final class Synced[+T](private[this] var x :Maybe[T]) extends Clearable[T] {
		override def get :T = synchronized(x.get)
		override def maybe :Maybe[T] = synchronized(x)
		override def clear() :Unit = synchronized { x = No }
	}

	@SerialVersionUID(Ver)
	private final class Volatile[+T](init :T) extends Clearable[T] {
		@volatile private[this] var x :Maybe[T] =  Yes(init)

		override def get :T = x.get
		override def maybe :Maybe[T] = x
		override def clear() :Unit = x = No
	}
}
