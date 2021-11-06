package net.noresttherein.slang.vars

import net.noresttherein.slang.funny.Initializer




/** Similar to [[net.noresttherein.slang.vars.Lazy.idempotent idempotent]] lazy value, but on serialization,
  * instead of evaluating the value and freeing the initializer expression for garbage collection,
  * it does the opposite: the evaluated value is `@transient` and the initializer is never dereferenced,
  * meaning it will be evaluated again on deserialization if the value is required. This is useful
  * if the value is large, not serializable, or if it references singleton values (which do not implement
  * aliasing after deserialization to ensure that only one instance exists, as Scala's singleton objects do).
  * @param init an initializer expression of a SAM type extending `() => T`, allowing use of literal
  *             expressions of the latter form as argument.
  * @author Marcin Mościcki
  */
@SerialVersionUID(1L)
final class Transient[+T] private (init :Initializer[T]) extends Lazy[T] with Serializable {
	@transient @scala.volatile private[this] var evaluated :T = _
	@transient private[this] var cached :T = _

	override def value :T = {
		var res = cached
		if (res == null) {
			res = evaluated
			if (res != null)
				cached = res
			else {
				res = init()
				cached = res
				evaluated = res
			}
		}
		res
	}

	override def isInitialized :Boolean = cached != null || evaluated != null

	override def map[O](f :T => O) :Lazy[O] =
		if (isInitialized) {
			val value = get
			Lazy.idempotent(f(value))
		} else
			Lazy.idempotent(f(value))

	override def flatMap[O](f :T => Lazy[O]) :Lazy[O] =
		if (isInitialized) {
			val value = get
			Lazy.idempotent(f(value).value)
		} else
			Lazy.idempotent(f(value).value)

	override def toString :String =
		if (cached != null) cached.toString
		else {
			val value = evaluated
			if (value != null) value.toString
			else "lazy(?)"
		}
}




/** A factory of simple wrappers of lazily initialized `@transient` values which are re-initialized after
  * serialization and deserialization.
  */
object Transient {
	//todo: a macro accepting idempotnent: => T

	/** Creates a lazily initialized value which may set it to the value of the by-name argument multiple times:
	  * both in case of a concurrent access by many threads, and
	  * @param idempotent an initializer expression of a serializable SAM type extending `() => T`,
	  *                   allowing use of literal expressions of the latter form as argument.
	  */
	def apply[T](idempotent :Initializer[T]) :Transient[T] = new Transient(idempotent)
}
