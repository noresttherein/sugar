package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.{InOutOrdering, SpecializedVars}
import net.noresttherein.sugar.vars.VolatileLike.{BoolVolatileLike, RefVolatileLike}




/** A boxed `@volatile` variable of type `T`. Volatile declaration allows it to be used concurrently, guaranteeing
  * that changes will become visible to other threads. Unlike with a plain `@volatile` variable,
  * [[net.noresttherein.sugar.vars.Volatile.testAndSet testAndSet]] and similar methods are atomic,
  * with `@volatile` access semantics regarding the ''happens-before'' relation of the memory model.
  * This makes this class a generic alternative to Java `AtomicXxx` family of classes, with semantics
  * of all operations (including assign mutators like `+=`, etc.) equivalent to the reference implementation
  * of the latter.
  *
  * For reference types, as well as in a generic context, all ''test-and-set'' and related operations
  * compare the referential equality, as per `eq`, rather than `equals`. This means that a $Ref created
  * by a generic method, and assigning a boxed `Integer`, will not compare the values of the integers,
  * but object identity, which is almost certainly not what you want. Properly `@specialized` variables
  * compare the values of the value types, as expected.
  * You may use `Volatile.`[[net.noresttherein.sugar.vars.Volatile.generic generic]] to create a proper specialization
  * for `Volatile[T]` based on a `ClassTag[T]`. If you need the atomic operations to implement true equality,
  * consider using [[net.noresttherein.sugar.vars.SyncVar SyncVar]]
  * or [[net.noresttherein.sugar.vars.SpinVar SpinVar]] instead.
  *
  * Volatile variables incur lower overhead than full java monitor synchronization, particularly with multiple
  * concurrent reads. However, if atomicity of more complex operations is required,
  * use [[net.noresttherein.sugar.vars.SyncVar SyncVar]] instead.
  * @tparam T the type of this variable
  * @define Ref `Volatile`
  * @define ref volatile variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
sealed class Volatile[@specialized(SpecializedVars) T] private[vars]
	extends VolatileLike[T] with Mutable[T] with Serializable
{
	@scala.volatile private[this] var x :T = _
	protected override def factory :Volatile.type = Volatile

	final override def value :T = x
	final override def value_=(newValue :T) :Unit = x = newValue

	override def mkString :String = mkString("Volatile")
}




/** Factory of boxed `@volatile` variables.
  * @define variable volatile reference variable
  */
@SerialVersionUID(Ver)
object Volatile extends VolatileLikeFactory[Volatile] {

	implicit def VolatileOrdering[T :Ordering] :Ordering[Volatile[T]] = new InOutOrdering[Volatile, T]


	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Volatile[T] = {
		val res = new Volatile[T]
		res.value = init
		res
	}
	protected override def newRefInstance[T](init :T) :Volatile[T] = new Ref[T](init)
	protected override def newBoolInstance(init :Boolean) :Volatile[Boolean] = new Bool(init)

	/** An unspecialized `Volatile` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `Volatile` (which would call `equals` on reference types,
	  * which we do not want).
	  */
	@SerialVersionUID(Ver)
	private class Ref[T](init :T) extends Volatile[T] with RefVolatileLike[T] { value = init }

	/** Optimised implementation of `Volatile[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	@SerialVersionUID(Ver)
	private class Bool(init :Boolean) extends Volatile[Boolean] with BoolVolatileLike { value = init }

}
