package net.noresttherein.sugar.vars

import java.lang.ref.{PhantomReference, Reference, ReferenceQueue, SoftReference, WeakReference}

import scala.annotation.unchecked.uncheckedVariance

import net.noresttherein.sugar.exceptions.{noSuch_!, unsupported_!}
import net.noresttherein.sugar.extensions.classNameMethods
import net.noresttherein.sugar.time.{Eternity, Milliseconds, MinusEternity, TimeInterval}
import net.noresttherein.sugar.vars.DisposableRef.SerializedEmptyRef
import net.noresttherein.sugar.vars.Maybe.{No, Yes}




/** A light wrapper over Java [[java.lang.ref.ReferenceQueue ReferenceQueue]] containing only instances
  * of [[net.noresttherein.sugar.vars.DisposableRef DisposableRef]], with API expressed in terms of the latter,
  * rather than Java [[java.lang.ref.Reference Reference]] (and using `Maybe`s instead of `null`s).
  */ //Cannot be Serializable, because internals of ReferenceQueue are not serializable.
class RefQueue[T](name :String) { //not serializable!
	def this() = this(null)

	private[vars] val underlying :ReferenceQueue[T] = new ReferenceQueue[T]

	private def unwrap[S <: T](ref :Reference[S]) :Maybe[DisposableRef[T]] =
		Maybe(ref.asInstanceOf[DisposableRef[S]])

	def poll :Maybe[DisposableRef[T]] = unwrap(underlying.poll)

	def remove() :Maybe[DisposableRef[T]] = unwrap(underlying.remove)

	def remove(timeout :Milliseconds) :Maybe[DisposableRef[T]] = unwrap(underlying.remove(timeout.toMillis))

	def remove(timeout :TimeInterval) :Maybe[DisposableRef[T]] = timeout match {
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
  * These classes are lighter than their standard Scala counterparts (do not require additional wrapping).
  * Additionally, this class is `Serializable`. However, it essentially behaves like a `@transient val`:
  * the referenced value is not serialized with the reference, and a deserialized $Ref is empty and without a queue.
  * This allows safe use of these values in `Serializable` types.
  * @define Ref `DisposableRef`
  * @define ref disposable reference
  * @author Marcin Mościcki
  */
sealed trait DisposableRef[+T] extends Ref[T] with Serializable { this : Reference[T @uncheckedVariance] =>
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
	@inline final override def const :T = unsupported_!(this.localClassName + ".const")

	/** Returns the referenced object, or throws [[NoSuchElementException]] if it has been garbage collected. */
	@inline final override def apply() :T = get

	/** Returns the referenced object, or throws [[NoSuchElementException]] if it has been garbage collected. */
	override def get :T = getOrNull match {
		case null => noSuch_!("Object was garbage collected")
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

	/** Returns `None`. */
	@inline final override def constOpt :Opt[T] = None

	/** The referenced object, unless garbage collected. */
	override def unsure :Unsure[T] = Unsure(getOrNull)

	/** Same as [[net.noresttherein.sugar.vars.DisposableRef.unsure unsure]]. */
	@inline final override def toUnsure :Unsure[T] = unsure

	/** Returns [[net.noresttherein.sugar.vars.Missing Missing]]. */
	@inline final override def unsureConst :Unsure[T] = Missing

	protected def factory :DisposableRefFactory[DisposableRef]
	protected[this] def writeReplace :AnyRef = new SerializedEmptyRef(factory)

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
	def unapply[T](ref :Reference[T]) :Maybe[DisposableRef[T]] = ref match {
		case null => No
		case ref :DisposableRef[T @unchecked] => Yes(ref)
		case _ => No
	}
	private class SerializedEmptyRef[T](factory :DisposableRefFactory[DisposableRef]) extends Serializable {
		private def readResolve :AnyRef = factory(null)
	}
}


/** Creates and extracts a value from a $Ref.
  * @define Ref `R`
  * @define ref disposable reference
  * @tparam R the associated reference kind.
  */
sealed trait DisposableRefFactory[+R[X] <: DisposableRef[X]] {
	/** Creates a $Ref not registered for any queue. */
	def apply[T](referent :T) :R[T]
	/** Creates a $Ref and registers it for the given queue. */
	def apply[T](referent :T, queue :RefQueue[T]) :R[T]
}




/** An adapter of Java [[java.lang.ref.WeakReference WeakReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  * @define Ref `WeakRef`
  * @define ref weak reference
  */
class WeakRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends WeakReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
	protected override def getOrNull :T = super[WeakReference].get
	protected override def factory :DisposableRefFactory[WeakRef] = WeakRef
}


/** Factory of and extractor of values from [[net.noresttherein.sugar.vars.WeakRef weak references]].
  * @define Ref `WeakRef`
  * @define ref weak reference
  */
case object WeakRef extends DisposableRefFactory[WeakRef] {
	@inline override def apply[T](referent :T) :WeakRef[T] = new WeakRef(referent)
	@inline override def apply[T](referent :T, queue :RefQueue[T]) :WeakRef[T] = new WeakRef[T](referent, queue.underlying)

	@inline def unapply[T](ref :WeakRef[T]) :Maybe[T] = ref.maybe
	@inline def unapply[T](ref :Reference[T]) :Maybe[T] = ref match {
		case null => No
		case ref :WeakRef[T] => ref.maybe
		case _ => No
	}
}




/** An adapter of Java [[java.lang.ref.SoftReference SoftReference]] to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]]
  * interface.
  * @define Ref `SoftRef`
  * @define ref soft reference
  */
class SoftRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends SoftReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])
	protected override def getOrNull :T = super[SoftReference].get
	protected override def factory :DisposableRefFactory[SoftRef] = SoftRef
}


/** Factory of and extractor of values from [[net.noresttherein.sugar.vars.SoftRef soft references]].
  * @define Ref `SoftRef`
  * @define ref soft reference
  */
case object SoftRef extends DisposableRefFactory[SoftRef] {
	@inline override def apply[T](referent :T) :SoftRef[T] = new SoftRef(referent)
	@inline override def apply[T](referent :T, queue :RefQueue[T]) :SoftRef[T] = new SoftRef(referent, queue.underlying)

	@inline def unapply[T](ref :SoftRef[T]) :Maybe[T] = ref.maybe
	@inline def unapply[T](ref :Reference[T]) :Maybe[T] = ref match {
		case null => No
		case ref :SoftRef[T] => ref.maybe
		case _ => No
	}
}




/** An adapter of Java [[java.lang.ref.PhantomReference PhantomReference]]
  * to `sugar` [[net.noresttherein.sugar.vars.Ref Ref]] interface.
  * @define Ref `PhantomRef`
  * @define ref phantom reference
  */
class PhantomRef[+T] private (referent :T, queue :ReferenceQueue[T])
	extends PhantomReference[T @uncheckedVariance](referent, queue) with DisposableRef[T]
{
	def this(referent :T, queue :RefQueue[T]) = this(referent, queue.underlying)
	def this(referent :T) = this(referent, null :ReferenceQueue[T])

	protected override def getOrNull :T = super.get

	protected override def factory :DisposableRefFactory[PhantomRef] = PhantomRef
	override def toString :String = "PhantomRef@" + Integer.toHexString(hashCode)
}


/** Factory of and extractor of values from [[net.noresttherein.sugar.vars.PhantomRef phantom references]].
  * @define Ref `PhantomRef`
  * @define ref phantom reference
  */
case object PhantomRef extends DisposableRefFactory[PhantomRef] {
	@inline override def apply[T](referent :T) :PhantomRef[T] = new PhantomRef(referent)
	@inline override def apply[T](referent :T, queue :RefQueue[T]) :PhantomRef[T] = new PhantomRef(referent, queue)
}
