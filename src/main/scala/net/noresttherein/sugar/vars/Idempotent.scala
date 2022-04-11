package net.noresttherein.sugar.vars

import java.lang.invoke.VarHandle.{acquireFence, releaseFence}

import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.Undefined




/** A lazy value initialized with an idempotent expression which may be evaluated more than once.
  * The advantage here is reduced synchronization overhead comparing to a Scala's standard `lazy val`,
  * even reduced to zero for value types once the value is initialized.
  * @author Marcin Mościcki
  */
trait Idempotent[@specialized(SpecializedVars) +T] extends Lazy[T]




object Idempotent {

	/** Creates a lazy value initialized - possibly multiple times - by a idempotent expression.
	  * This class does not use a `synchronized` block, yielding a minor performance benefit
	  * while still remaining thread safe. It happens at the cost of possibly evaluating the initialization expression
	  * more than once, and concurrent access of an uninitialized value may return results from different calls.
	  * For this reason the initializer should be a relatively lightweight and '''idempotent''' function.
	  * For standard value types, access to an initialized value incurs no synchronization overhead,
	  * although this might be partially offset by unnecessary repeated execution of the initializer after stale reads.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isDefined`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	def apply[@specialized(SpecializedVars) T](idempotent: => T) :Idempotent[T] = {
		val initializer = () => idempotent
		new IdempotentVal(initializer) match {
			case ref if ref.getClass == classOf[IdempotentVal[Any]] => new IdempotentRef(initializer)
			case spec => spec
		}
	}

	/** A wrapper over an already computed value adapting it to the `Idempotent` type. It is used
	  * by [[net.noresttherein.sugar.vars.Lazy.map map]] and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]]
	  * methods and as a serialization substitute.
	  */
	def eager[@specialized(SpecializedVars) T](value :T) :Idempotent[T] = new EagerIdempotent[T](value)



	/** A full `Idempotent` lazy value implementation as a mix-in trait for application classes,
	  * in particular various lazy proxies.
	  */
	trait AbstractIdempotent[@specialized(SpecializedVars) +T] extends Idempotent[T] {
		@scala.volatile protected[this] var initializer :() => T
		@scala.volatile private[this] var evaluated :T = _

		override def isDefined: Boolean = { val init = initializer; acquireFence(); init == null }

		override def opt :Opt[T] =
			if (initializer == null)
				Lack
			else {
				acquireFence()
				Got(evaluated)
			}

		override def get :T = {
			val init = initializer
			acquireFence()
			if (init == null)
				evaluated
			else {
				val res = init()
				evaluated = res
				releaseFence()
				initializer = null
				res
			}
		}

		override def map[O](f :T => O) :Lazy[O] = {
			val init = initializer
			acquireFence()
			if (init == null)
				eager(f(evaluated))
			else {
				val t = init()
				evaluated = t
				releaseFence()
				initializer = null
				new IdempotentRef(() => f(t))
			}
		}

		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val init = initializer
			acquireFence()
			if (init == null)
				f(evaluated)
			else
				new IdempotentRef(f(apply()))
		}

		override def isSpecialized = false

		override def toString :String =
			if (initializer == null) { acquireFence(); String.valueOf(evaluated) }
			else Undefined.toString//"Lazy(?)"
	}



	/** An already computed (initialized) value. */
	@SerialVersionUID(1L) //Not specialized so we don't box the value type to fit in an Opt all the time
	private class EagerIdempotent[+T](x :T) extends Idempotent[T] {
		override def isDefined :Boolean = true
		override def get :T = x

		override def map[O](f :T => O) :Lazy[O] = new EagerIdempotent[O](f(x))
		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = f(x)

		override def isSpecialized :Boolean = getClass == classOf[EagerIdempotent[_]]
	}



	/** Nothing specialized in this implementation, it only guarantees that `T` is a primitive/immutable wrapper,
	  * which allows more lax synchronisation.
	  */
	@SerialVersionUID(1L)
	private class IdempotentVal[@specialized(SpecializedVars) +T](private[this] var initializer : () => T)
		extends Idempotent[T]
	{
		private[this] var evaluated :Any = Undefined

		override def isDefined: Boolean = evaluated != Undefined

		override def opt :Opt[T] = {
			val res = evaluated
			if (res == Undefined) Lack else Got(res.asInstanceOf[T])
		}

		@unspecialized override def get: T = {
			var res = evaluated
			if (res == Undefined) {
				val init = initializer
				acquireFence()
				if (init == null)
					res = evaluated
				else {
					res = init()
					evaluated = res
					releaseFence()
					initializer = null
				}
			}
			res.asInstanceOf[T]
		}

		@unspecialized override def map[O](f: T => O): Lazy[O] = {
			val v = evaluated
			if (v != Undefined)
				eager(f(v.asInstanceOf[T]))
			else {
				val init = initializer
				acquireFence()
				if (init == null)
					eager(f(evaluated.asInstanceOf[T]))
				else
					new IdempotentRef(() => f(apply()))
			}
		}
		@unspecialized override def flatMap[O](f: T => Lazy[O]): Lazy[O] = {
			val v = evaluated
			if (v != Undefined)
				f(v.asInstanceOf[T])
			else {
				val init = initializer
				acquireFence()
				if (init == null)
					f(evaluated.asInstanceOf[T])
				else
					new IdempotentRef(f(apply()))
			}
		}

		override def isSpecialized = true

		override def toString :String = String.valueOf(evaluated)
//			if (evaluated != Undefined) String.valueOf(evaluated)
//			else if (initializer == null) { acquireFence(); String.valueOf(evaluated) }
//			else "Lazy(?)"

		private def writeReplace = eager(apply())
	}



	/** `Idempotent` implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
	  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
	  */
	@SerialVersionUID(1L)
	private[vars] class IdempotentRef[T](private[this] var initializer :() => T) extends Idempotent[T] {
		private[this] var evaluated :T = _

		override def isDefined: Boolean = { val init = initializer; acquireFence(); init == null }

		override def opt :Opt[T] =
			if (initializer == null)
				Lack
			else {
				acquireFence()
				Got(evaluated)
			}

		override def get :T = {
			val init = initializer
			acquireFence()
			if (init == null)
				evaluated
			else {
				val res = init()
				evaluated = res
				releaseFence()
				initializer = null
				res
			}
		}

		override def map[O](f :T => O) :Lazy[O] = {
			val init = initializer
			acquireFence()
			if (init == null)
				eager(f(evaluated))
			else {
				val t = init()
				evaluated = t
				releaseFence()
				initializer = null
				new IdempotentRef(() => f(t))
			}
		}

		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val init = initializer
			acquireFence()
			if (init == null)
				f(evaluated)
			else
				new IdempotentRef(f(apply()))
		}

		override def isSpecialized = false

		override def toString :String =
			if (initializer == null) { acquireFence(); String.valueOf(evaluated) }
			else Undefined.toString//"Lazy(?)"

		private def writeReplace = eager(apply())
	}

}
