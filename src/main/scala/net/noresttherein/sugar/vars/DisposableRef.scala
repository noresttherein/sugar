package net.noresttherein.sugar.vars

import java.lang.ref.{PhantomReference, Reference, ReferenceQueue, SoftReference, WeakReference}

import scala.annotation.unchecked.uncheckedVariance

import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.time.{Eternity, Milliseconds, MinusEternity, TimeInterval}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** A light wrapper over Java [[java.lang.ref.ReferenceQueue ReferenceQueue]] containing only instances
  * of [[net.noresttherein.sugar.vars.DisposableRef DisposableRef]], with API expressed in terms of the latter,
  * rather than Java [[java.lang.ref.Reference Reference]] (and using `Opt`s instead of `null`s).
  */
class RefQueue[T](name :String) { //not serializable!
	def this() = this(null)

	private[vars] val underlying :ReferenceQueue[T] = new ReferenceQueue[T]

	private def unwrap[S <: T](ref :Reference[S]) :Opt[DisposableRef[T]] =
		Opt(ref.asInstanceOf[DisposableRef[S]])

	def poll :Opt[DisposableRef[T]] = unwrap(underlying.poll)

	def remove() :Opt[DisposableRef[T]] = unwrap(underlying.remove)

	def remove(timeout :Milliseconds) :Opt[DisposableRef[T]] = unwrap(underlying.remove(timeout.toMillis))

	def remove(timeout :TimeInterval) :Opt[DisposableRef[T]] = timeout match {
		case Eternity => remove()
		case MinusEternity => poll
		case _ => remove(timeout.asMillis)
	}

	override def toString :String =
		if (name != null) "RefQueue(" + name + ")"
		else "RefQueue@" + Integer.toHexString(hashCode)
}




/** An adaptation of Java [[java.lang.ref.Reference Reference]] class hierarchy
  * to [[net.noresttherein.sugar.vars.Ref Ref]] interface.
  * @define Ref `DisposableRef`
  * @author Marcin Mościcki
  */ //not a Serializable because Reference is not a Serializable
sealed trait DisposableRef[+T] extends Ref[T] { this : Reference[T @uncheckedVariance] =>
	def toReference :Reference[_ <: T] = this

	protected def getOrNull :T

	/** Returns [[net.noresttherein.sugar.vars.DisposableRef.isEmpty isEmpty]]. */
	override def isFinal :Boolean = getOrNull == null

	/** Returns `false`, as the reference is not immutable. */
	override def isFinalizable :Boolean = false

	/** Returns `false`. */
	override def isConst    :Boolean = false

	/** Checks if the reference has been cleared and the referenced object garbage collected. */
	override def isEmpty    :Boolean = getOrNull == null

	/** The reference still holds its value. */
	override def isDefined  :Boolean = getOrNull != null

	/** The reference still holds its value. */
	override def isDefinite :Boolean = getOrNull != null

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.get get]]. */
	@inline final override def value :T = getOrNull

	/** Throws [[UnsupportedOperationException]]. */
	@inline final override def const :T = throw new UnsupportedOperationException(this.localClassName + ".const")

	/** Returns the referenced object, or throws [[NoSuchElementException]] if it has been garbage collected. */
	@inline final override def apply() :T = get

	/** Returns the referenced object, or throws [[NoSuchElementException]] if it has been garbage collected. */
	override def get :T = getOrNull match {
		case null => throw new NoSuchElementException("Object was garbage collected")
		case x => x
	}

	/** The referenced object, unless garbage collected. */
	override def option :Option[T] = Option(getOrNull)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.option option]]. */
	@inline final override def toOption :Option[T] = option

	/** Returns `None`. */
	@inline final override def constOption :Option[T] = None

	/** The referenced object, unless garbage collected. */
	override def opt :Opt[T] = Opt(getOrNull)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.opt opt]]. */
	@inline final override def toOpt :Opt[T] = opt

	/** Returns [[net.noresttherein.sugar.vars.Opt.Lack Lack]]. */
	@inline final override def constOpt :Opt[T] = Lack

	/** The referenced object, unless garbage collected. */
	override def unsure :Unsure[T] = Unsure(getOrNull)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.unsure unsure]]. */
	@inline final override def toUnsure :Unsure[T] = unsure

	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]]. */
	@inline final override def constUnsure :Unsure[T] = Missing


	private[vars] override def isSpecialized = false

	override def equals(that :Any) :Boolean = this eq that.asInstanceOf[AnyRef]
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[DisposableRef[_]]
	override def hashCode :Int = System.identityHashCode(this)

	override def toString :String = getOrNull match {
		case null => "<clear>"
		case v => String.valueOf(v)
	}
}


object DisposableRef {
	def unapply[T](ref :Reference[T]) :Opt[DisposableRef[T]] = ref match {
		case null => Lack
		case ref :DisposableRef[T @unchecked] => Got(ref)
		case _ => Lack
	}
}




/** An adapter of Java [[java.lang.ref.WeakReference WeakReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  */
class WeakRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends WeakReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
	protected override def getOrNull :T = super[WeakReference].get
}


object WeakRef {
	@inline def apply[T](referent :T) :WeakRef[T] = new WeakRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :WeakRef[T] = new WeakRef[T](referent, queue.underlying)

	@inline def unapply[T](ref :WeakRef[T]) :Opt[T] = ref.opt
	@inline def unapply[T](ref :Reference[T]) :Opt[T] = ref match {
		case null => Lack
		case ref :WeakRef[T] => ref.opt
		case _ => Lack
	}
}




/** An adapter of Java [[java.lang.ref.SoftReference SoftReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  */
class SoftRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends SoftReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
	protected override def getOrNull :T = super[SoftReference].get
}


object SoftRef {
	@inline def apply[T](referent :T) :SoftRef[T] = new SoftRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :SoftRef[T] = new SoftRef(referent, queue.underlying)

	@inline def unapply[T](ref :SoftRef[T]) :Opt[T] = ref.opt
	@inline def unapply[T](ref :Reference[T]) :Opt[T] = ref match {
		case null => Lack
		case ref :SoftRef[T] => ref.opt
		case _ => Lack
	}
}




/** An adapter of Java [[java.lang.ref.PhantomReference PhantomReference]]
  * to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]] interface.
  */
class PhantomRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends PhantomReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])

	protected override def getOrNull :T = super.get
	override def toString :String = "PhantomRef@" + Integer.toHexString(hashCode)
}


object PhantomRef {
	@inline def apply[T](referent :T) :PhantomRef[T] = new PhantomRef(referent)
	@inline def apply[T](referent :T, queue :RefQueue[T]) :PhantomRef[T] = new PhantomRef(referent, queue)
}
