package net.noresttherein.sugar.vars

import scala.Specializable.Args

import net.noresttherein.sugar.time.{Eternity, Immediate, Milliseconds, TimeInterval}
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




/** An externally set, synchronized value where every read blocks until it is initialized. All calls to `value` acquire
  * the monitor for this object and, if `!this.isInitialized`, will wait until `this.notifyAll()` is called
  * by `this.value = x`. The value can be set only once, with an [[IllegalStateException]] being thrown on subsequent
  * attempts. For this reason, all update methods which depend on a preexistent value will fail
  * with an [[UnsupportedOperationException]].
  * @see [[net.noresttherein.sugar.vars.SignalVar]]
  */
@SerialVersionUID(1L)
final class SignalVal[T] private extends InOut[T] with Val[T] {
	@volatile private[this] var x :Opt[T] = Lack

	override def isDefined :Boolean = x.isDefined
	override def opt :Opt[T] = x


	def await(timeout :Milliseconds) :Boolean =
		x.isDefined || synchronized {
			x.isDefined || { wait(timeout.toMillis, 0); x.isDefined }
		}

	def await(timeout :TimeInterval) :Boolean =
		x.isDefined || timeout <= Immediate || synchronized {
			x.isDefined || (timeout match {
				case Eternity => wait(); x.isDefined
				case _ => wait(timeout.toSeconds, timeout.nanos); x.isDefined
			})
		}

	override def value :T = x match {
		case Got(v) => v
		case _ => synchronized {
			while (x.isEmpty) wait()
			x.get
		}
	}
	override def value_=(newValue :T) :Unit = synchronized {
		if (isDefined)
			throw new IllegalStateException(s"Cannot set SignalVal(${x.get}) to $newValue: already initialized.")
		else {
			x = Got(newValue)
			notifyAll()
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


	private[vars] override def isSpecialized = false

	override def hashCode :Int = super[Val].hashCode

	override def toString :String = synchronized {
		if (isDefined) String.valueOf(value)
		else "SignalVal(?)@" + Integer.toHexString(System.identityHashCode(this))
	}
}




object SignalVal {
	/** Creates a new, uninitialized [[net.noresttherein.sugar.vars.SignalVal SignalVal]] instance. */
	def apply[T] :SignalVal[T] = new SignalVal[T]

	/** Creates a new, uninitialized [[net.noresttherein.sugar.vars.SignalVal SignalVal]] instance.
	  * It is exactly equivalent to `SignalVal[T]`, but reads better than `SignalVal.apply` when
	  * the type parameter is omitted for the compiler to infer.
	  */
	def empty[T] :SignalVal[T] = new SignalVal[T]
}
