package net.noresttherein.sugar.vars

import java.lang.invoke.VarHandle.{acquireFence, releaseFence}

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.undefined




/** A lazy value initialized with an idempotent expression which may be evaluated more than once.
  * The advantage here is reduced synchronization overhead comparing to a Scala's standard `lazy val`,
  * even reduced to zero for value types once the value is initialized.
  * @define Ref `Pure`
  * @define ref pure value
  * @author Marcin Mościcki
  */
trait Pure[@specialized(SpecializedVars) +T] extends Lazy[T] {
	override def mkString :String = mkString("Pure")
}




@SerialVersionUID(Ver)
object Pure {

	/** Creates a lazy value initialized - possibly multiple times - by an idempotent expression.
	  * This class does not use a `synchronized` block, yielding a minor performance benefit
	  * while still remaining thread safe. It happens at the cost of possibly evaluating the initialization expression
	  * more than once, and concurrent access of an uninitialized value may return results from different calls.
	  * For this reason the initializer should be a relatively lightweight and '''idempotent''' function.
	  * By ''idempotent'' we mean here that the values returned by repeated calls should be ''equal''
	  * in terms of Scala `==`, that is, `equals` for reference types. The function does ''not'' need to
	  * return the same object all the time, that is its returned values need not be ''referentially'' equal
	  * (in terms of `eq`). This should be taken into account by client applications, which should not depend
	  * on subsequent calls to [[net.noresttherein.sugar.vars.Val.get get]].
	  *
	  * For standard value types, access to an initialized value incurs no synchronization overhead,
	  * although this might be partially offset by unnecessary repeated execution of the initializer after stale reads.
	  * All testing functions become even less helpful, but at least it is guaranteed that once `isDefinite`
	  * returns `true`, it will always be so (in the sense of the java memory 'happens before' relation).
	  */
	def apply[@specialized(SpecializedVars) T](idempotent: => T) :Pure[T] = {
		val initializer = () => idempotent
		new PureVal[T] match {
			case ref if ref.getClass == classOf[Val[Any]] => new PureRef(initializer)
			case spec =>
				spec.init(initializer)
				spec
		}
	}

	/** A wrapper over an already computed value adapting it to the `Pure` type. It is used
	  * by [[net.noresttherein.sugar.vars.Lazy.map map]] and [[net.noresttherein.sugar.vars.Lazy.flatMap flatMap]]
	  * methods and as a serialization substitute.
	  */
	def eager[@specialized(SpecializedVars) T](value :T) :Pure[T] = new Eager[T](value)



	/** An already computed (initialized) value. */ //todo: make it specialized
	@SerialVersionUID(Ver) //Not specialized so we don't box the value type to fit in an Opt all the time
	private class Eager[+T](x :T) extends Pure[T] {
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
@SerialVersionUID(Ver)
private class PureVal[@specialized(SpecializedVars) +T]
	extends Pure[T]
{
	private[this] var initializer : () => T = _
	private[this] var evaluated :Any = undefined

	def init(init :() => T @uncheckedVariance) :Unit = initializer = init

	override def isDefinite :Boolean = evaluated != undefined

	@unspecialized override def value :T = {
		val res = evaluated
		if (res != undefined) res.asInstanceOf[T]
		else throw new NoSuchElementException("Uninitialized Pure")
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
				//res is a primitive wrapper (like Integer) whose field is final,
				// so anyone reading evaluated is guaranteed to either see its previous value, or a valid box.
				// Moreover, because intializer is assumed to be idempotent, i.e return equal instances,
				// the reader does not care if they see a previous (equal) box, or the current one.
				evaluated = res
				releaseFence() //Any thread observing initializer == null must see evaluated = res
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
			Pure.eager(f(v.asInstanceOf[T]))
		else {
			val init = initializer
			acquireFence()
			if (init == null)
				Pure.eager(f(evaluated.asInstanceOf[T]))
			else
				new PureRef(() => f(apply()))
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
				new PureRef(f(apply()))
		}
	}

	override def isSpecialized = true

	override def mkString(prefix :String) :String = evaluated match {
		case `undefined` => prefix + "()"
		case v => prefix + "(" + v + ")"
	}
	override def toString :String = String.valueOf(evaluated)

	private def writeReplace = Pure.eager(apply())
}




/** $Ref implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
  */
@SerialVersionUID(Ver)
private class PureRef[T](private[this] var initializer :() => T) extends Pure[T] {
	@volatile private[this] var evaluated :T = _

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
			throw new NoSuchElementException("Uninitialized Pure")

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
			Pure.eager(f(evaluated))
		else {
			val t = init()
			evaluated = t
			releaseFence()
			initializer = null
			new PureRef(() => f(t))
		}
	}

	override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
		val init = initializer
		acquireFence()
		if (init == null)
			f(evaluated)
		else
			new PureRef(f(apply()))
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

	private def writeReplace = Pure.eager(apply())
}





/** A full `Pure` lazy value implementation as a mix-in trait for application classes,
  * in particular various lazy proxies.
  */
private[sugar] trait AbstractPure[@specialized(SpecializedVars) +T] {
	protected[this] var initializer :() => T
	@volatile private[this] var evaluated :T = _

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
			throw new NoSuchElementException("Uninitialized " + this)

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
