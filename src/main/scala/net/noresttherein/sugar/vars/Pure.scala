package net.noresttherein.sugar.vars

import scala.annotation.unspecialized

import net.noresttherein.sugar.concurrent.Fences.{acquireFence, releaseFence}
import net.noresttherein.sugar.exceptions.noSuch_!
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.One




/** A lazy value initialized with an idempotent expression which may be evaluated more than once.
  * However, if the values returned by repeated executions of the initializer are only `equal`,
  * but not referentially equivalent (as in `eq`), different reads may potentially see different objects.
  * The advantage here is reduced synchronization overhead comparing to a Scala's standard `lazy val`,
  * even reduced to zero for value types once the value is initialized.
  * @define Ref `Pure`
  * @define ref pure value
  * @author Marcin Mościcki
  */
trait Pure[@specialized(SpecializedVars) +T] extends Delayed[T] {
	//todo: pureMap, pureFlatMap
	override def mkString :String = mkString("Pure")
}




@SerialVersionUID(Ver)
case object Pure {

	/** Creates a lazy value initialized - possibly multiple times - by an idempotent expression.
	  * This class does not use a `synchronized` block, yielding a minor performance benefit
	  * while still remaining thread safe. It happens at the cost of possibly evaluating the initialization expression
	  * more than once, and concurrent access of an uninitialized value may return results from different calls.
	  * For this reason the initializer should be a relatively lightweight and '''idempotent''' function.
	  * By ''idempotent'', we mean here that the values returned by repeated calls should be ''equal''
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
	@inline def apply[@specialized(SpecializedVars) T](idempotent: => T) :Pure[T] = from(() => idempotent)

	/** Same as [[net.noresttherein.sugar.vars.Pure$.apply apply]], but accepts a `Function0`,
	  * rather a by-name parameter.
	  */
	def from[@specialized(SpecializedVars) T](initializer: () => T) :Pure[T] =
		new PureVal[T](initializer) match {
			case ref if ref.getClass == classOf[PureVal[Any]] => new PureRef(initializer)
			case spec => spec
		}

	/** A wrapper over an already computed value adapting it to the `Pure` type. It is used
	  * by [[net.noresttherein.sugar.vars.Delayed.map map]] and [[net.noresttherein.sugar.vars.Delayed.flatMap flatMap]]
	  * methods and as a serialization substitute.
	  */
	def eager[@specialized(SpecializedVars) T](value :T) :Pure[T] = new Eager[T](value)



	/** An already computed (initialized) value. */ //todo: make it specialized
	@SerialVersionUID(Ver) //Not specialized so we don't box the value type to fit in a Maybe all the time
	private class Eager[+T](x :T) extends Pure[T] {
		override def isDefinite :Boolean = true
		override def value :T = x
		override def get   :T = x

		override def map[O](f :T => O) :Delayed[O] = new Eager[O](f(x))
		override def flatMap[O](f :T => Delayed[O]) :Delayed[O] = f(x)

		override def isSpecialized :Boolean = false //getClass == classOf[Eager[_]]
	}

}




/** Nothing specialized in this implementation, it only guarantees that `T` is a primitive/immutable wrapper,
  * which allows more lax synchronisation. We however rely on it being `@specialized` to recognize that we should
  * use `PureRef` instead.
  */
@SerialVersionUID(Ver)
private class PureVal[@specialized(SpecializedVars) +T](private[this] var content :Any) extends Pure[T] {

	override def isDefinite :Boolean = !content.isInstanceOf[() => _]

	@unspecialized override def value :T = content match {
		case _ :(() => Any)    => noSuch_!("Uninitialized Pure")
		case res :T @unchecked => res
	}
	@unspecialized override def get :T = content match {
		case init :(() => T) @unchecked =>
			acquireFence()
			val res = init()
			content = res
			res
		case res :T @unchecked => res
	}

	override def option :Option[T] = content match {
		case _ :(() => Any)      => None
		case res :T @unchecked => Some(res)
	}
	override def opt :Opt[T] = content match {
		case _ :(() => Any)      => None
		case res :T @unchecked => One(res)
	}
	override def unsure :Unsure[T] = content match {
		case _ :(() => Any)      => Missing
		case res :T @unchecked => Sure(res)
	}

	@unspecialized override def map[O](f :T => O) :Delayed[O] = content match {
		case _ :(() => Any)    => new PureRef(() => f(apply()))
		case v :T @unchecked => Pure.eager(f(v))
	}
	@unspecialized override def flatMap[O](f :T => Delayed[O]) :Delayed[O] = content match {
		case _ :(() => Any)    => new PureRef(f(apply()))
		case v :T @unchecked => f(v)
	}

	override def isSpecialized = true

	private def writeReplace :AnyRef = Pure.eager(apply())
}




/** $Ref implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
  */
@SerialVersionUID(Ver)
private class PureRef[T](private[this] var initializer :() => T) extends Pure[T] {
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
			None
		else {
			acquireFence()
			One(evaluated)
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
			noSuch_!("Uninitialized Pure")

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

	override def map[O](f :T => O) :Delayed[O] = {
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

	override def flatMap[O](f :T => Delayed[O]) :Delayed[O] = {
		val init = initializer
		acquireFence()
		if (init == null)
			f(evaluated)
		else
			new PureRef(f(apply()))
	}

	override def isSpecialized = false

	private def writeReplace :AnyRef = Pure.eager(apply())
}
