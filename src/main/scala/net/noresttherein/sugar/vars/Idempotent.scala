package net.noresttherein.sugar.vars

import java.lang.invoke.VarHandle.{acquireFence, releaseFence}

import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.undefined




/** A lazy value initialized with an idempotent expression which may be evaluated more than once.
  * The advantage here is reduced synchronization overhead comparing to a Scala's standard `lazy val`,
  * even reduced to zero for value types once the value is initialized.
  * @define Ref `Idempotent`
  * @author Marcin Mościcki
  */
trait Idempotent[@specialized(SpecializedVars) +T] extends Lazy[T] {
	override def mkString :String = mkString("Idempotent")
}




@SerialVersionUID(Ver)
object Idempotent {

	/** Creates a lazy value initialized - possibly multiple times - by an idempotent expression.
	  * This class does not use a `synchronized` block, yielding a minor performance benefit
	  * while still remaining thread safe. It happens at the cost of possibly evaluating the initialization expression
	  * more than once, and concurrent access of an uninitialized value may return results from different calls.
	  * For this reason the initializer should be a relatively lightweight and '''idempotent''' function.
	  * For standard value types, access to an initialized value incurs no synchronization overhead,
	  * although this might be partially offset by unnecessary repeated execution of the initializer after stale reads.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isDefinite`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	def apply[@specialized(SpecializedVars) T](idempotent: => T) :Idempotent[T] = {
		val initializer = () => idempotent
		new IdempotentVal(initializer) match {
			case ref if ref.getClass == classOf[Val[Any]] => new IdempotentRef(initializer)
			case spec => spec
		}
	}

	/** A wrapper over an already computed value adapting it to the `Idempotent` type. It is used
	  * by [[net.noresttherein.sugar.vars.Lazy.map map]] and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]]
	  * methods and as a serialization substitute.
	  */
	def eager[@specialized(SpecializedVars) T](value :T) :Idempotent[T] = new Eager[T](value)



	/** An already computed (initialized) value. */ //todo: make it specialized
	@SerialVersionUID(Ver) //Not specialized so we don't box the value type to fit in an Opt all the time
	private class Eager[+T](x :T) extends Idempotent[T] {
		override def isDefinite  :Boolean = true
		override def value    :T = x
		override def const :T = x

		override def map[O](f :T => O) :Lazy[O] = new Eager[O](f(x))
		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = f(x)

		override def isSpecialized :Boolean = false //getClass == classOf[Eager[_]]
	}

}




/** Nothing specialized in this implementation, it only guarantees that `T` is a primitive/immutable wrapper,
  * which allows more lax synchronisation.
  */
@SerialVersionUID(Ver) //todo: make it specialized
private class IdempotentVal[@specialized(SpecializedVars) +T](private[this] var initializer : () => T)
	extends Idempotent[T]
{	//no need for fences because T is a value type
	private[this] var evaluated :Any = undefined

	override def isDefinite :Boolean = evaluated != undefined

	@unspecialized override def value :T = {
		val res = evaluated
		if (res != undefined) res.asInstanceOf[T]
		else throw new NoSuchElementException("Uninitialized Idempotent")
	}
	@unspecialized override def const :T = {
		var res = evaluated
		if (res == undefined) {
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

	override def option :Option[T] = {
		val res = evaluated
		if (res == undefined) None else Some(res.asInstanceOf[T])
	}
	override def opt :Opt[T] = {
		val res = evaluated
		if (res == undefined) Lack else Got(res.asInstanceOf[T])
	}
	override def unsure :Unsure[T] = {
		val res = evaluated
		if (res == undefined) Missing else Sure(res.asInstanceOf[T])
	}

	@unspecialized override def map[O](f :T => O) :Lazy[O] = {
		val v = evaluated
		if (v != undefined)
			Idempotent.eager(f(v.asInstanceOf[T]))
		else {
			val init = initializer
			acquireFence()
			if (init == null)
				Idempotent.eager(f(evaluated.asInstanceOf[T]))
			else
				new IdempotentRef(() => f(apply()))
		}
	}
	@unspecialized override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
		val v = evaluated
		if (v != undefined)
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

	override def mkString(prefix :String) :String = evaluated match {
		case `undefined` => prefix + "()"
		case v => prefix + "(" + v + ")"
	}
	override def toString :String = String.valueOf(evaluated)

	private def writeReplace = Idempotent.eager(apply())
}




/** `Idempotent` implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
  */
@SerialVersionUID(Ver)
private class IdempotentRef[T](private[this] var initializer :() => T) extends Idempotent[T] {
	private[this] var evaluated :T = _

	override def isDefinite: Boolean = { val init = initializer; acquireFence(); init == null }

	override def option :Option[T] =
		if (initializer == null)
			None
		else {
			acquireFence()
			Some(evaluated)
		}
	override def opt :Opt[T] =
		if (initializer == null)
			Lack
		else {
			acquireFence()
			Got(evaluated)
		}
	override def unsure :Unsure[T] =
		if (initializer == null)
			Missing
		else {
			acquireFence()
			Sure(evaluated)
		}

	override def value :T =
		if (initializer == null) {
			acquireFence()
			evaluated
		} else
			throw new NoSuchElementException("Uninitialized Idempotent")

	override def const :T = {
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
			Idempotent.eager(f(evaluated))
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

	override def mkString(prefix :String) =
		if (initializer == null) {
			acquireFence()
			prefix + "(" + evaluated + ")"
		} else prefix + "()"
	override def toString :String =
		if (initializer == null) { acquireFence(); String.valueOf(evaluated) }
		else undefined.toString

	private def writeReplace = Idempotent.eager(apply())
}





/** A full `Idempotent` lazy value implementation as a mix-in trait for application classes,
  * in particular various lazy proxies.
  */
private[sugar] trait AbstractIdempotent[@specialized(SpecializedVars) +T] {
	protected[this] var initializer :() => T
	private[this] var evaluated :T = _

	@inline final def isDefinite: Boolean = { val init = initializer; acquireFence(); init == null }

	def ? :Opt[T] =
		if (initializer != null)
			Lack
		else {
			acquireFence()
			Got(evaluated)
		}
	def unsure :Unsure[T] =
		if (initializer != null)
			Missing
		else {
			acquireFence()
			Sure(evaluated)
		}

	def indefinite :T =
		if (initializer == null) {
			acquireFence()
			evaluated
		} else
			throw new NoSuchElementException("Uninitialized Idempotent")

	def definite :T = {
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
}

