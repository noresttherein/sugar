package net.noresttherein.sugar.vars

import scala.Specializable.Args
import net.noresttherein.sugar.time.{Eternity, Immediate, Milliseconds, TimeInterval}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.SignalVal.MappedSignalVal




/** An externally set, synchronized value every read of which blocks until it is initialized. All calls to `value`
  * will, if `this.`[[net.noresttherein.sugar.vars.SignalVal.isEmpty isEmpty]], block until another thread calls
  * `this.value = x`. The value can be set only once, and an [[IllegalStateException]] is thrown on subsequent attempts.
  * For this reason, all update methods which depend on a preexistent value
  * will fail with an [[UnsupportedOperationException]].
  * @see [[net.noresttherein.sugar.vars.Out]]
  * @see [[net.noresttherein.sugar.vars.SignalVar]]
  * @define Ref `SignalVal`
  */
@SerialVersionUID(Ver) //consider: SignalRef (different equality semantics)
sealed class SignalVal[T] private extends InOut[T] with Val[T] { //todo: extend Out
	@volatile private[this] var x :Opt[T] = Lack

	def await(timeout: Milliseconds) :Boolean =
		x.isDefined || synchronized {
			x.isDefined || {
				wait(timeout.toMillis, 0); x.isDefined
			}
		}

	def await(timeout: TimeInterval) :Boolean =
		x.isDefined || timeout <= Immediate || synchronized {
			x.isDefined || (timeout match {
				case Eternity => wait(); x.isDefined
				case _ => wait(timeout.toSeconds, timeout.nanos); x.isDefined
			})
		}

	override def isEmpty       :Boolean = !isDefined
	override def isFinalizable :Boolean = true
	override def isConst       :Boolean = isDefined
	override def isDefined     :Boolean = opt.isDefined
	override def isDefinite    :Boolean = opt.isDefined

	override def value :T = opt match {
		case Got(v) => v
		case _ => throw new NoSuchElementException(toString + ".value")
	}
	/** Sets the value of this `SignalVal` and notifies all threads waiting for it using one of
	  * [[net.noresttherein.sugar.vars.SignalVal.await await]] methods. After the value is set,
	  * all calls to [[net.noresttherein.sugar.vars.SignalVal.value value]],
	  * [[net.noresttherein.sugar.vars.SignalVal.get get]] and [[net.noresttherein.sugar.vars.SignalVal.const const]]
	  * will return it without blocking. This method can be called only once:
	  * all subsequent attempts will throw an [[IllegalStateException]].
	  */
	override def value_=(newValue :T) :Unit = synchronized {
		if (isDefined)
			throw new IllegalStateException(s"Cannot set SignalVal(${x.get}) to $newValue: already initialized.")
		else {
			x = Got(newValue)
			notifyAll()
		}
	}
	override def get   :T = value
	override def const :T = constOpt.get

	override def opt      :Opt[T] = x
	override def toOpt    :Opt[T] = opt
	override def constOpt :Opt[T] = x match {
		case res if res.isDefined => res
		case _ => synchronized {
			while (x.isEmpty) wait()
			x
		}
	}

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def ?=(newValue :T) :T =
		throw new UnsupportedOperationException("SignalVal can't be set multiple times.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def testAndSet(expect :T, newValue :T) :Boolean =
		throw new UnsupportedOperationException("Cannot testAndSet a SignalVal")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def apply(f :T => T) :T =
		throw new UnsupportedOperationException("SignalVal cannot be modified.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def applyLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T =
		throw new UnsupportedOperationException("SignalVal cannot be modified.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def applyRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T =
		throw new UnsupportedOperationException("SignalVal cannot be modified.")


	/** A new `SignalVal` with a value derived from this one.
	  * If `this.`[[net.noresttherein.sugar.vars.SignalVal isDefined]], then the new instance is initialized
	  * to `f(this.value)` before being returned. Otherwise all [[net.noresttherein.sugar.vars.SignalVal.await await]]
	  * methods of the result wait on this instance's monitor rather than the new `SignalVal`'s,
	  * and all getters such as [[net.noresttherein.sugar.vars.SignalVal.value value]],
	  * [[net.noresttherein.sugar.vars.SignalVal.get get]], [[net.noresttherein.sugar.vars.SignalVal.const const]]
	  * and their optional variants defer to this instance.
	  */
	override def map[O](f :T => O) :SignalVal[O] = x match {
		case Got(defined) =>
			val res = new SignalVal[O]
			res.value = f(defined)
			res
		case _ =>
			new MappedSignalVal(this, f)
	}

	private[vars] override def isSpecialized = false

	override def toString :String = x match {
		case Got(v) => String.valueOf(v)
		case _ => "<undefined>@" + Integer.toHexString(System.identityHashCode(this))
	}
}




@SerialVersionUID(Ver)
object SignalVal {
	/** Creates a new, uninitialized [[net.noresttherein.sugar.vars.SignalVal SignalVal]] instance. */
	def apply[T] :SignalVal[T] = new SignalVal[T]

	/** Creates a new, uninitialized [[net.noresttherein.sugar.vars.SignalVal SignalVal]] instance.
	  * It is exactly equivalent to `SignalVal[T]`, but reads better than `SignalVal.apply` when
	  * the type parameter is omitted for the compiler to infer.
	  */
	def empty[T] :SignalVal[T] = new SignalVal[T]

	implicit def signalValOrdering[T :Ordering] :Ordering[SignalVal[T]] = Val.valOrdering


	@SerialVersionUID(Ver)
	private class MappedSignalVal[T, O](source :SignalVal[T], f :T => O) extends SignalVal[O] {
		override def await(timeout :Milliseconds): Boolean = source.await(timeout)
		override def await(timeout :TimeInterval): Boolean = source.await(timeout)

		override def value_=(newValue: O) :Unit =
			throw new UnsupportedOperationException("Cannot manually set the value of a mapped SignalVal " + this + ".")

		override def opt: Opt[O] = {
			val local = super.opt
			if (local.isDefined)
				local
			else
				result(source.opt)
		}
		override def constOpt :Opt[O] = {
			val local = super.opt
			if (local.isDefined)
				local
			else
				result(source.constOpt)
		}
		private def result(outer :Opt[T]) =
			if (outer.isDefined) {
				val res = f(outer.get)
				synchronized {
					if (super.opt.isEmpty)
						super.value_=(res)
				}
				Got(res)
			} else
				Lack
	}

}
