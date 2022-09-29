package net.noresttherein.sugar.vars

import scala.annotation.unspecialized

import net.noresttherein.sugar.vars.Idempotent.IdempotentRef
import net.noresttherein.sugar.vars.InOut.SpecializedVars
import net.noresttherein.sugar.vars.Opt.{Got, Lack}
import net.noresttherein.sugar.vars.Ref.undefined




/** Similar to `Lazy.`[[net.noresttherein.sugar.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
  * instead of evaluating the value and freeing the initializer expression for garbage collection,
  * it does the opposite: the evaluated value is `@transient` and the initializer's reference is never cleared,
  * meaning it will be evaluated again on deserialization if the value is required. This is useful
  * if the value is large, not serializable, or if it references singleton values (which do not implement
  * aliasing after deserialization to ensure that only one instance exists, as Scala's singleton objects do).
  * @define Ref `Transient`
  * @author Marcin Mościcki
  */
sealed trait Transient[@specialized(SpecializedVars) +T] extends Idempotent[T] {
	override def mkString :String = mkString("Transient")
}




/** A factory of simple wrappers of lazily initialized `@transient` values which are re-initialized after
  * serialization and deserialization.
  */
@SerialVersionUID(ver)
object Transient {
	//todo: a macro accepting idempotent: => T

	/** Creates a lazily initialized value which may set it to the value of the by-name argument multiple times:
	  * both in case of a concurrent access by many threads, and after deserialization.
	  * @param idempotent an initializer expression of a serializable SAM type extending `() => T`,
	  *                   allowing use of literal expressions of the latter form as argument.
	  */
	def apply[@specialized(SpecializedVars) T](idempotent :Eval[T]) :Transient[T] = {
		new TransientVal(idempotent) match {
			case ref if ref.getClass == classOf[TransientVal[Any]] => new TransientRef(idempotent)
			case spec => spec
		}
	}



	/** Nothing specialized in this implementation, it only guarantees that `T` is a primitive/immutable wrapper,
	  * which allows more lax synchronisation.
	  */
	@SerialVersionUID(ver) //todo: make it really specialized
	private class TransientVal[@specialized(SpecializedVars) +T](initializer :Eval[T]) extends Transient[T] {
		@transient private[this] var evaluated :Any = undefined

		override def isEmpty :Boolean = evaluated == undefined

		override def option :Option[T] = {
			val res = evaluated
			if (res == undefined) None else Some(res.asInstanceOf[T])
		}
		override def opt :Opt[T] = {
			val res = evaluated
			if (res == undefined) Lack else Got(res.asInstanceOf[T])
		}
		@unspecialized override def unsure :Unsure[T] = {
			val res = evaluated
			if (res == undefined) Missing else Sure(res.asInstanceOf[T])
		}

		@unspecialized override def value :T = {
			val res = evaluated
			if (res == undefined) res.asInstanceOf[T]
			else throw new NoSuchElementException("Uninitialized Transient")
		}
		@unspecialized override def const :T = {
			var res = evaluated
			if (res == undefined) {
				res = initializer()
				evaluated = res
			}
			res.asInstanceOf[T]
		}

		@unspecialized override def map[O](f :T => O) :Lazy[O] = {
			val v = evaluated
			if (v != undefined) {
				val res = f(v.asInstanceOf[T])
				new TransientRef(() => res)
			} else
				new IdempotentRef(() => f(apply()))
		}

		@unspecialized override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val v = evaluated
			if (v != undefined)
				f(v.asInstanceOf[T])
			else
				new IdempotentRef(() => f(apply()))
		}

		override def isSpecialized = true

		override def toString :String = String.valueOf(evaluated)
	}



	/** `Transient` implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
	  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
	  */
	@SerialVersionUID(ver)
	private class TransientRef[T](initializer :Eval[T]) extends Transient[T] {
		@transient @volatile private[this] var evaluated :Any = _

		override def isEmpty :Boolean = evaluated == undefined

		override def option :Option[T] = {
			val res = evaluated
			if (res == undefined) None else Some(evaluated.asInstanceOf[T])
		}
		override def opt :Opt[T] = {
			val res = evaluated
			if (res == undefined) Lack else Got(evaluated.asInstanceOf[T])
		}
		override def unsure :Unsure[T] = {
			val res = evaluated
			if (res == undefined) Missing else Unsure(evaluated.asInstanceOf[T])
		}

		override def value :T = {
			val res = evaluated
			if (res != undefined) res.asInstanceOf[T]
			else throw new NoSuchElementException("Uninitialized Transient")
		}
		override def const :T = {
			var res = evaluated
			if (res == undefined) {
				res = initializer()
				evaluated = res
			}
			res.asInstanceOf[T]
		}

		override def map[O](f :T => O) :Lazy[O] = {
			val v = evaluated
			if (v != undefined) {
				val res = f(v.asInstanceOf[T])
				new TransientRef(() => res)
			} else
				new IdempotentRef(() => f(apply()))
		}

		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val v = evaluated
			if (v != undefined)
				f(v.asInstanceOf[T])
			else
				new IdempotentRef(f(apply()))
		}

		override def isSpecialized = false

		override def toString :String = String.valueOf(evaluated)
	}

}
