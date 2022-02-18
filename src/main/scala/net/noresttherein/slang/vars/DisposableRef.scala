package net.noresttherein.slang.vars

import java.lang.ref.{PhantomReference, Reference, ReferenceQueue, SoftReference, WeakReference}

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.{Got, Lack}
import net.noresttherein.slang.time.{Eternity, Milliseconds, MinusEternity, TimeSpan}
import net.noresttherein.slang.vars.DisposableRef.WrapperReference
import net.noresttherein.slang.vars.PhantomRef.WrappedPhantomRef
import net.noresttherein.slang.vars.SoftRef.WrappedSoftRef
import net.noresttherein.slang.vars.WeakRef.WrappedWeakRef






/** A light wrapper over Java [[java.lang.ref.ReferenceQueue ReferenceQueue]] containing only instances
  * of [[net.noresttherein.slang.vars.DisposableRef DisposableRef]], with API expressed in terms of the latter,
  * rather than Java [[java.lang.ref.Reference Reference]] (and using [[Option]]s instead of `null`s).
  */
class RefQueue[T](name :String) {
	def this() = this(null)

	private[vars] val underlying :ReferenceQueue[T] = new ReferenceQueue[T]

	private def unwrap[S <: T](ref :Reference[S]) :Option[DisposableRef[T]] =
		if (ref == null) None
		else Some(ref.asInstanceOf[WrapperReference[S]].self)

	def poll :Option[DisposableRef[T]] = unwrap(underlying.poll)

	def remove() :Option[DisposableRef[T]] = unwrap(underlying.remove)

	def remove(timeout :Milliseconds) :Option[DisposableRef[T]] = unwrap(underlying.remove(timeout.inMillis))

	def remove(timeout :TimeSpan) :Option[DisposableRef[T]] = timeout match {
		case Eternity => remove()
		case MinusEternity => poll
		case _ => remove(timeout.asMillis)
	}

	override def toString :String = if (name != null) name else "RefQueue@" + Integer.toHexString(hashCode)
}




/** An adaptation of Java [[java.lang.ref.Reference Reference]] class hierarchy
  * to [[net.noresttherein.slang.vars.Ref Ref]] interface.
  * @author Marcin Mościcki
  */
sealed class DisposableRef[+T](referent :T, queue :ReferenceQueue[T])
                              (cons :(T, ReferenceQueue[T], DisposableRef[T]) => Reference[T])
	extends Ref[T]
{
	private def underlying :Reference[_ <: T] = ref
	private[this] val ref = cons(referent, queue, this)
//	private[this] var callbacks :List[() => Unit] = Nil

	override def apply() :T = ref.get match {
		case null => throw new NoSuchElementException("Object was garbage collected")
		case x => x
	}
	override def opt :Opt[T]    = Opt(ref.get)
	override def get :Option[T] = Option(ref.get)

//	def onClean(f: => Unit) :Unit = synchronized { callbacks = (() => f) :: callbacks}

	def isEnqueued :Boolean = ref.isEnqueued
	def enqueue() :Boolean = ref.enqueue()
	def clear() :Unit = ref.clear()

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :DisposableRef[_] if other canEqual this => underlying == other.underlying
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[DisposableRef[_]]
	override def hashCode :Int = ref.hashCode

	override def toString :String = String.valueOf(ref.get)
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




/** An adapter of Java [[java.lang.ref.WeakReference WeakReference]] to `slang` [[net.noresttherein.slang.vars.Ref Ref]]
  * interface.
  */
@SerialVersionUID(1L)
class WeakRef[+T](referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedWeakRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
}


object WeakRef {
	@inline def apply[T](referent :T) :WeakRef[T] = new WeakRef(referent)
	@inline def unapply[T](ref :WeakRef[T]) :Opt[T] = ref.opt
	@inline def unapply[T](ref :Reference[T]) :Opt[T] = ref match {
		case null => Lack
		case wrapper :WrappedWeakRef[T] => wrapper.self.opt
		case _ => Lack
	}

	private class WrappedWeakRef[T](referent :T, queue :ReferenceQueue[T], override val self :DisposableRef[T])
		extends WeakReference[T](referent, queue) with WrapperReference[T]
}




/** An adapter of Java [[java.lang.ref.SoftReference SoftReference]] to `slang` [[net.noresttherein.slang.vars.Ref Ref]]
  * interface.
  */
@SerialVersionUID(1L)
class SoftRef[+T](referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedSoftRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
}


object SoftRef {
	@inline def apply[T](referent :T) :SoftRef[T] = new SoftRef(referent)
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
  * to `slang` [[net.noresttherein.slang.vars.Ref Ref]] interface.
  */
@SerialVersionUID(1L)
class PhantomRef[+T](referent :T, queue :ReferenceQueue[T])
	extends DisposableRef[T](referent, queue)(new WrappedPhantomRef[T](_, _, _))
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])

	override def toString :String = "PhantomRef@" + Integer.toHexString(hashCode)
}


object PhantomRef {
	@inline def apply[T](referent :T) :PhantomRef[T] = new PhantomRef(referent)

	private class WrappedPhantomRef[T](referent :T, queue :ReferenceQueue[T], override val self :DisposableRef[T])
		extends PhantomReference[T](referent, queue) with WrapperReference[T]
}



