package net.noresttherein.sugar.vars

import scala.Specializable.Args

import net.noresttherein.sugar.time.{Eternity, Immediate, Milliseconds, TimeInterval}
import net.noresttherein.sugar.{illegalState_!, noSuch_!, unsupported_!}
import net.noresttherein.sugar.vars.Maybe.{No, Yes}
import net.noresttherein.sugar.vars.SignalVal.Mapped




/** An externally set, synchronized value every read of which blocks until it is initialized. All calls to `value`
  * will, if `this.`[[net.noresttherein.sugar.vars.SignalVal.isEmpty isEmpty]], block until another thread calls
  * `this.value = x`. The value can be set only once, and an [[IllegalStateException]] is thrown on subsequent attempts.
  * For this reason, all update methods which depend on a preexistent value
  * will fail with an [[UnsupportedOperationException]].
  * @see [[net.noresttherein.sugar.vars.Out]]
  * @see [[net.noresttherein.sugar.vars.SignalVar]]
  * @define Ref `SignalVal`
  * @define ref signal value
  */
@SerialVersionUID(Ver) //consider: SignalRef (different equality semantics)
sealed class SignalVal[T] private extends InOut[T] with Val[T] with Serializable { //todo: extend Out
	@volatile private[this] var x :Maybe[T] = No

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
	override def isDefined     :Boolean = maybe.isDefined
	override def isDefinite    :Boolean = maybe.isDefined

	override def value :T = maybe match {
		case Yes(v) => v
		case _      => noSuch_!(toString + ".value")
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
			illegalState_!(s"Cannot set SignalVal(${x.get}) to $newValue: already initialized.")
		else {
			x = Yes(newValue)
			notifyAll()
		}
	}
	override def get   :T = value
	override def const :T = maybeConst.get

	override def maybe      :Maybe[T] = x
	override def toMaybe    :Maybe[T] = maybe
	override def maybeConst :Maybe[T] = x match {
		case res if res.isDefined => res
		case _ => synchronized {
			while (x.isEmpty) wait()
			x
		}
	}


	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def ?=(newValue :T) :T =
		unsupported_!("SignalVal can't be set multiple times.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def testAndSet(expect :T, newValue :T) :Boolean =
		unsupported_!("Cannot testAndSet a SignalVal")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def update(f :T => T) :T =
		unsupported_!("SignalVal cannot be modified.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def updateLeft[@specialized(Args) A](z :A)(f :(A, T) => T) :T =
		unsupported_!("SignalVal cannot be modified.")

	/** Throws [[UnsupportedOperationException]]. */
	@throws[UnsupportedOperationException]
	override def updateRight[@specialized(Args) A](z :A)(f :(T, A) => T) :T =
		unsupported_!("SignalVal cannot be modified.")


	/** A new `SignalVal` with a value derived from this one.
	  * If `this.`[[net.noresttherein.sugar.vars.SignalVal isDefined]], then the new instance is initialized
	  * to `f(this.value)` before being returned. Otherwise all [[net.noresttherein.sugar.vars.SignalVal.await await]]
	  * methods of the result wait on this instance's monitor rather than the new `SignalVal`'s,
	  * and all getters such as [[net.noresttherein.sugar.vars.SignalVal.value value]],
	  * [[net.noresttherein.sugar.vars.SignalVal.get get]], [[net.noresttherein.sugar.vars.SignalVal.const const]]
	  * and their optional variants defer to this instance.
	  */
	override def map[O](f :T => O) :SignalVal[O] = x match {
		case Yes(defined) =>
			val res = new SignalVal[O]
			res.value = f(defined)
			res
		case _ =>
			new Mapped(this, f)
	}

	private[vars] override def isSpecialized = false

	override def mkString :String = mkString("SignalVal")

	override def toString :String = x match {
		case Yes(v) => String.valueOf(v)
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
	private class Mapped[T, O](source :SignalVal[T], f :T => O) extends SignalVal[O] {
		override def await(timeout :Milliseconds): Boolean = source.await(timeout)
		override def await(timeout :TimeInterval): Boolean = source.await(timeout)

		override def value_=(newValue: O) :Unit =
			unsupported_!("Cannot manually set the value of a mapped SignalVal " + this + ".")

		override def maybe: Maybe[O] = {
			val local = super.maybe
			if (local.isDefined)
				local
			else
				result(source.maybe)
		}
		override def maybeConst :Maybe[O] = {
			val local = super.maybe
			if (local.isDefined)
				local
			else
				result(source.maybeConst)
		}
		private def result(outer :Maybe[T]) =
			if (outer.isDefined) {
				val res = f(outer.get)
				synchronized {
					if (super.maybe.isEmpty)
						super.value_=(res)
				}
				Yes(res)
			} else
				No
	}
}
