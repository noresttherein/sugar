package net.noresttherein.sugar.vars

import java.lang.ref.{PhantomReference, Reference, ReferenceQueue, SoftReference, WeakReference}

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.time.{Eternity, Milliseconds, MinusEternity, TimeInterval}
import net.noresttherein.sugar.vars.DisposableRef.WrapperReference
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.PhantomRef.WrappedPhantomRef
import net.noresttherein.sugar.vars.SoftRef.WrappedSoftRef
import net.noresttherein.sugar.vars.WeakRef.WrappedWeakRef




/** A light wrapper over Java [[java.lang.ref.ReferenceQueue ReferenceQueue]] containing only instances
  * of [[net.noresttherein.sugar.vars.DisposableRef DisposableRef]], with API expressed in terms of the latter,
  * rather than Java [[java.lang.ref.Reference Reference]] (and using `Option`s instead of `null`s).
  */
class RefQueue[T](name :String) {
	def this() = this(null)

	private[vars] val underlying :ReferenceQueue[T] = new ReferenceQueue[T]

	private def unwrap[S <: T](ref :Reference[S]) :Option[DisposableRef[T]] =
		if (ref == null) None
		else Some(ref.asInstanceOf[WrapperReference[S]].self)

	def poll :Option[DisposableRef[T]] = unwrap(underlying.poll)

	def remove() :Option[DisposableRef[T]] = unwrap(underlying.remove)

	def remove(timeout :Milliseconds) :Option[DisposableRef[T]] = unwrap(underlying.remove(timeout.toMillis))

	def remove(timeout :TimeInterval) :Option[DisposableRef[T]] = timeout match {
		case Eternity => remove()
		case MinusEternity => poll
		case _ => remove(timeout.asMillis)
	}

	override def toString :String = if (name != null) name else "RefQueue@" + Integer.toHexString(hashCode)
}




/** An adaptation of Java [[java.lang.ref.Reference Reference]] class hierarchy
  * to [[net.noresttherein.sugar.vars.Ref Ref]] interface.
	* @define Ref `DisposableRef`
  * @author Marcin Mościcki
  */ //not a Serializable because Reference is not a Serializable
sealed class DisposableRef[+T] protected (referent :T, queue :ReferenceQueue[T])
                                         (cons :(T, ReferenceQueue[T], DisposableRef[T]) => Reference[T])
	extends Ref[T]
{
	private[this] val ref = cons(referent, queue, this)

	/** Returns [[net.noresttherein.sugar.vars.DisposableRef.isEmpty isEmpty]]. */
	override def isFinal :Boolean = ref.get == null

	/** Returns `false`, as the reference is not immutable. */
	override def isFinalizable :Boolean = false

	/** Returns `false`. */
	override def isConst    :Boolean = false

	/** Checks if the reference has been cleared and the referenced object garbage collected. */
	override def isEmpty    :Boolean = ref.get == null

	/** The reference still holds its value. */
	override def isDefined  :Boolean = ref.get != null

	/** The reference still holds its value. */
	override def isDefinite :Boolean = ref.get != null

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.get get]]. */
	@inline final override def value :T = get

	/** Throws [[UnsupportedOperationException]]. */
	@inline final override def const :T = throw new UnsupportedOperationException(this.localClassName + ".const")

	@inline final override def apply() :T = get

	override def get :T = ref.get match {
		case null => throw new NoSuchElementException("Object was garbage collected")
		case x => x
	}

	/** The referenced object, unless garbage collected. */
	override def option :Option[T] = Option(ref.get)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.option option]]. */
	@inline final override def toOption :Option[T] = option

	/** Returns `None`. */
	@inline final override def constOption :Option[T] = None

	/** The referenced object, unless garbage collected. */
	override def opt :Opt[T] = Opt(ref.get)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.opt opt]]. */
	@inline final override def toOpt :Opt[T] = opt

	/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]]. */
	@inline final override def constOpt :Opt[T] = Lack

	/** The referenced object, unless garbage collected. */
	override def unsure :Unsure[T] = Unsure(ref.get)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.unsure unsure]]. */
	@inline final override def toUnsure :Unsure[T] = unsure

	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]]. */
	@inline final override def constUnsure :Unsure[T] = Missing


	/** Adds the underlying reference to its [[java.lang.ref.ReferenceQueue ReferenceQueue]]. */
	def enqueue() :Boolean = ref.enqueue()

	/** Clears the reference, allowing the referenced object to be garbage collected. */
	def clear() :Unit = ref.clear()


	private[vars] override def isSpecialized = false

	override def equals(that :Any) :Boolean = this eq that.asInstanceOf[AnyRef]
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[DisposableRef[_]]
	override def hashCode :Int = System.identityHashCode(this)

	override def toString :String = ref.get match {
		case null => "<clear>"
		case v => String.valueOf(v)
	}
}


object DisposableRef {
	def unapply[T](ref :Reference[T]) :Opt[DisposableRef[T]] = ref match {
		case null => Lack
		case wrapper :WrapperReference[T] => Got(wrapper.self)
		case _ => Lack
	}

	private[vars] trait WrapperReference[T] extends Reference[T] {
		val self :DisposableRef[T]
	}
}




/** An adapter of Java [[java.lang.ref.WeakReference WeakReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  */
class WeakRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedWeakRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
}


object WeakRef {
	@inline def apply[T](referent :T) :WeakRef[T] = new WeakRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :WeakRef[T] = new WeakRef[T](referent, queue.underlying)

	@inline def unapply[T](ref :WeakRef[T]) :Opt[T] = ref.opt
	@inline def unapply[T](ref :Reference[T]) :Opt[T] = ref match {
		case null => Lack
		case wrapper :WrappedWeakRef[T] => wrapper.self.opt
		case _ => Lack
	}

	private class WrappedWeakRef[T](referent :T, queue :ReferenceQueue[T], override val self :DisposableRef[T])
		extends WeakReference[T](referent, queue) with WrapperReference[T]
}




/** An adapter of Java [[java.lang.ref.SoftReference SoftReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  */
class SoftRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedSoftRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
}


object SoftRef {
	@inline def apply[T](referent :T) :SoftRef[T] = new SoftRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :SoftRef[T] = new SoftRef(referent, queue.underlying)

	@inline def unapply[T](ref :SoftRef[T]) :Opt[T] = ref.opt
	@inline def unapply[T](ref :Reference[T]) :Opt[T] = ref match {
		case null => Lack
		case wrapper :WrappedSoftRef[T] => wrapper.self.opt
		case _ => Lack
	}

	private class WrappedSoftRef[T](referent :T, queue :ReferenceQueue[T], override val self :DisposableRef[T])
		extends SoftReference[T](referent, queue) with WrapperReference[T]
}




/** An adapter of Java [[java.lang.ref.PhantomReference PhantomReference]]
  * to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]] interface.
  */
class PhantomRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedPhantomRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])

	override def toString :String = "PhantomRef@" + Integer.toHexString(hashCode)
}


object PhantomRef {
	@inline def apply[T](referent :T) :PhantomRef[T] = new PhantomRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :PhantomRef[T] = new PhantomRef(referent, queue)

	private class WrappedPhantomRef[T](referent :T, queue :ReferenceQueue[T], override val self :DisposableRef[T])
		extends PhantomReference[T](referent, queue) with WrapperReference[T]
}
