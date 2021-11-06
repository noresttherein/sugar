package net.noresttherein.slang.vars

import scala.Specializable.Args

import net.noresttherein.slang.optional.Opt
import net.noresttherein.slang.optional.Opt.{Got, Lack}




/** An externally set, synchronized value where every read blocks until it is initialized. All calls to `value` acquire
  * the monitor for this object and, if `!this.isInitialized`, will wait until `this.notifyAll()` is called
  * by `this.value = x`. The value can be set only once, with an [[IllegalStateException]] being thrown on subsequent
  * attempts. For this reason, all update methods which depend on a preexistent value will fail
  * with a [[UnsupportedOperationException]].
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
final class SignalVal[T] private extends InOut[T] with Val[T] {
	private[this] var opt :Opt[T] = Lack

	@inline override def isInitialized :Boolean = synchronized { opt.isDefined }

	override def toOpt :Opt[T] = opt

	override def value :T = synchronized {
		while (opt.isEmpty) wait()
		opt.get
	}

	override def value_=(value :T) :Unit = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Cannot set SignalVal(${opt.get}) to $value: already initialized.")
		else {
			opt = Got(value)
			notifyAll()
		}
	}

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def ?=(value :T) :T =
		throw new UnsupportedOperationException("SignalVal can't be set multiple times.")


	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def testAndSet(expect :T, assign :T) :Boolean =
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


	override def toString :String = synchronized {
		if (isInitialized) String.valueOf(value)
		else "SignalVal(?)@" + System.identityHashCode(this)
	}
}




object SignalVal {
	def apply[T]() :SignalVal[T] = new SignalVal[T]
}
