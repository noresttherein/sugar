package net.noresttherein.slang.vars

import scala.annotation.unspecialized

import net.noresttherein.slang.funny.Initializer
import net.noresttherein.slang.vars.Idempotent.IdempotentRef
import net.noresttherein.slang.vars.InOut.SpecializedVars
import net.noresttherein.slang.vars.Opt.{Got, Lack}
import net.noresttherein.slang.vars.Ref.Undefined




/** Similar to `Lazy.`[[net.noresttherein.slang.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
  * instead of evaluating the value and freeing the initializer expression for garbage collection,
  * it does the opposite: the evaluated value is `@transient` and the initializer's reference is never cleared,
  * meaning it will be evaluated again on deserialization if the value is required. This is useful
  * if the value is large, not serializable, or if it references singleton values (which do not implement
  * aliasing after deserialization to ensure that only one instance exists, as Scala's singleton objects do).
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
sealed trait Transient[@specialized(SpecializedVars) +T] extends Idempotent[T]




/** A factory of simple wrappers of lazily initialized `@transient` values which are re-initialized after
  * serialization and deserialization.
  */
object Transient {
	//todo: a macro accepting idempotent: => T

	/** Creates a lazily initialized value which may set it to the value of the by-name argument multiple times:
	  * both in case of a concurrent access by many threads, and after deserialization.
	  * @param idempotent an initializer expression of a serializable SAM type extending `() => T`,
	  *                   allowing use of literal expressions of the latter form as argument.
	  */
	def apply[@specialized(SpecializedVars) T](idempotent :Initializer[T]) :Transient[T] = {
		new TransientVal(idempotent) match {
			case ref if ref.getClass == classOf[TransientVal[Any]] => new TransientRef(idempotent)
			case spec => spec
		}
	}



	/** Nothing specialized in this implementation, it only guarantees that `T` is a primitive/immutable wrapper,
	  * which allows more lax synchronisation.
	  */
	@SerialVersionUID(1L)
	private class TransientVal[@specialized(SpecializedVars) +T](initializer :Initializer[T]) extends Transient[T] {
		@transient private[this] var evaluated :Any = Undefined

		override def isDefined: Boolean = evaluated != Undefined

		override def opt :Opt[T] = {
			val res = evaluated
			if (res == null) Lack else Got(res.asInstanceOf[T])
		}

		@unspecialized override def get: T = {
			var res = evaluated
			if (res == Undefined) {
				res = initializer()
				evaluated = res
			}
			res.asInstanceOf[T]
		}

		@unspecialized override def map[O](f: T => O): Lazy[O] = {
			val v = evaluated
			if (v != Undefined) {
				val res = f(v.asInstanceOf[T])
				new TransientRef(() => res)
			} else
				new IdempotentRef(() => f(apply()))
		}

		@unspecialized override def flatMap[O](f: T => Lazy[O]): Lazy[O] = {
			val v = evaluated
			if (v != Undefined)
				f(v.asInstanceOf[T])
			else
				new IdempotentRef(() => f(apply()))
		}

		override def isSpecialized = true

		override def toString :String = String.valueOf(evaluated)
//			if (evaluated != Undefined) String.valueOf(evaluated)
//			else "lazy(?)"
	}



	/** `Transient` implementation for arbitrary types. All reads are behind an `acquireFence`, while initialization
	  * completes with a `releaseFence` to ensure that `evaluated` is never visible in a partially initialized state.
	  */
	@SerialVersionUID(1L)
	private class TransientRef[T](initializer :Initializer[T]) extends Transient[T] {
		@transient @volatile private[this] var evaluated :Any = _

		override def isDefined: Boolean = evaluated != Undefined

		override def opt :Opt[T] = {
			val res = evaluated
			if (res == Undefined) Lack else Got(evaluated.asInstanceOf[T])
		}

		override def get :T = {
			var res = evaluated
			if (res == Undefined) {
				res = initializer()
				evaluated = res
			}
			res.asInstanceOf[T]
		}

		override def map[O](f :T => O) :Lazy[O] = {
			val v = evaluated
			if (v != Undefined) {
				val res = f(v.asInstanceOf[T])
				new TransientRef(() => res)
			} else
				new IdempotentRef(() => f(apply()))
		}

		override def flatMap[O](f :T => Lazy[O]) :Lazy[O] = {
			val v = evaluated
			if (v != Undefined)
				f(v.asInstanceOf[T])
			else
				new IdempotentRef(f(apply()))
		}

		override def isSpecialized = false

		override def toString :String = evaluated.toString
//		override def toString :String = {
//			val v = evaluated
//			if (v != Undefined) String.valueOf(v)
//			else "lazy(?)"
//		}
	}

}
