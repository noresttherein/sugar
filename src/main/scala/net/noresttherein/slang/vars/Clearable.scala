package net.noresttherein.slang.vars

import java.lang.ref.Cleaner.Cleanable

import net.noresttherein.slang.vars.Opt.{Got, Lack}




/** `Clearable` is the opposite of [[net.noresttherein.slang.vars.Out Out]]: a value box initialized on construction,
  * but with its contents being subject to being cleared - unreferenced and freed for a garbage collector.
  * It is not a [[net.noresttherein.slang.vars.DisposableRef DisposableRef]] as it uses a hard reference
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
  *             init.asOption match {
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
  * @author Marcin Mościcki
  */
trait Clearable[+T] extends Ref[T] with AutoCloseable with Cleanable with Serializable {

	/** Checks if this variable currently holds a value. Not that,
	  * unlike with [[net.noresttherein.slang.vars.Lazy Lazy]], this property is `true` when the object is created
	  * and, at some point, may become `false` and remains so for the remainder of this object's life.
	  * It makes this method of very dubious utility, as any positive value can be outdated before even
	  * it is returned to the caller. It can however still be used as a flag signaling that some other variable
	  * is initialized, if the initialization of the latter is synchronized with clearing of this object.
	  * In order to access the value, use [[net.noresttherein.slang.vars.Ref.opt opt]],
	  * [[net.noresttherein.slang.vars.Ref.asOption asOption]] or [[net.noresttherein.slang.vars.Ref.unsure unsure]].
	  */
	@inline final override def isDefined :Boolean = opt.isDefined

	/** Resets this variable to an undefined state, unreferencing its contents. */
	def clear() :Unit

	/** Resets this variable to an undefined state, unreferencing its contents.
	  * Same as [[net.noresttherein.slang.vars.Clearable.clear clear]].
	  * This is an [[AutoCloseable]] method.
	  */
	override def close() :Unit = clear()

	/** Resets this variable to an undefined state, unreferencing its contents.
	  * Same as [[net.noresttherein.slang.vars.Clearable.clear clear]].
	  * This is a [[java.lang.ref.Cleaner.Cleanable Cleanable]] method.
	  */
	override def clean() :Unit = clear()
}




object Clearable {
	/** An unsynchronized, non thread safe `Clearable` variable initialized with the given value. */
	def apply[T](value :T) :Clearable[T] = new PlainClearable[T](Got(value))

	/** A `Clearable` variable initialized with the given value, synchronizing all access on its monitor. */
	def sync[T](value :T) :Clearable[T] = new SynchronizedClearable(Got(value))

	/** A `Clearable` instance backed by a `@volatile` variable. */
	def volatile[T](value :T) :Clearable[T] = new VolatileClearable(value)


	private class PlainClearable[+T](private[this] var x :Opt[T]) extends Clearable[T] {
		override def get :T = x.get
		override def opt :Opt[T] = x
		override def clear() :Unit = x = Lack
	}

	private final class SynchronizedClearable[+T](private[this] var x :Opt[T]) extends Clearable[T] {
		override def get :T = synchronized(x.get)
		override def opt :Opt[T] = x
		override def clear() :Unit = x = Lack
	}

	private final class VolatileClearable[+T](init :T) extends Clearable[T] {
		@volatile private[this] var x :Opt[T] =  Got(init)

		override def get :T = x.get
		override def opt :Opt[T] = x
		override def clear() :Unit = x = Lack
	}
}
