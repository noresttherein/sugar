package net.noresttherein.sugar.vars

import java.lang.ref.{PhantomReference, Reference, ReferenceQueue, SoftReference, WeakReference}

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
  * @author Marcin Mościcki
  */ //not a Serializable because Reference is not a Serializable
sealed class DisposableRef[+T] protected (referent :T, queue :ReferenceQueue[T])
                                         (cons :(T, ReferenceQueue[T], DisposableRef[T]) => Reference[T])
	extends Ref[T]
{
	private def underlying :Reference[_ <: T] = ref
	private[this] val ref = cons(referent, queue, this)
//	private[this] var callbacks :List[() => Unit] = Nil

	override def isDefined :Boolean = ref.get != null

	override def get :T = ref.get match {
		case null => throw new NoSuchElementException("Object was garbage collected")
		case x => x
	}
	override def ? :Option[T] = Option(ref.get)
	override def opt :Opt[T] = Opt(ref.get)
	override def unsure :Unsure[T] = Unsure(ref.get)

//	def onClean(f: => Unit) :Unit = synchronized { callbacks = (() => f) :: callbacks}


//	def isEnqueued :Boolean = ref.isEnqueued
	def enqueue() :Boolean = ref.enqueue()
	def clear() :Unit = ref.clear()


	private[vars] override def isSpecialized = false

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :DisposableRef[_] if other canEqual this => underlying == other.underlying
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[DisposableRef[_]]
	override def hashCode :Int = ref.hashCode

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



