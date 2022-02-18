package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.SpecializedVars
import net.noresttherein.slang.vars.VolatileLike.{BoolVolatileLike, RefVolatileLike}




/** A boxed `@volatile` variable of type `T`. Volatile declaration allows it to be used concurrently, guaranteeing
  * that changes will become visible to other threads. Unlike with a plain `@volatile` variable,
  * [[net.noresttherein.slang.vars.Volatile.testAndSet testAndSet]] and similar methods are atomic,
  * with `@volatile` access semantics regarding the ''happens-before'' relation of the memory model.
  * This makes this class a polymorphic alternative to Java `AtomicXxx` family of classes, with semantics
  * of all operations (including assign mutators like `+=`, etc.) equivalent to the reference implementation
  * of the latter.
  *
  * Volatile variables incur lower overhead than full java monitor synchronization, particularly with multiple
  * concurrent reads. However, if atomicity of more complex operations is required,
  * use [[net.noresttherein.slang.vars.SyncVar SyncVar]] instead.
  * @param init the initial value of this variable.
  * @tparam T the type of this variable
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Volatile[@specialized(SpecializedVars) T] private[vars] (init :T)
	extends InOut[T] with VolatileLike[T] with Serializable
{
	@scala.volatile private[this] var x = init
	protected override def factory :Volatile.type = Volatile

	final override def value :T = x

	final override def value_=(newValue :T) :Unit = x = newValue
}




/** Factory of boxed `@volatile` variables.
  * @define variable volatile reference variable
  */
object Volatile extends VolatileLikeFactory[Volatile] {

	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Volatile[T] = new Volatile(init)
	protected override def newRefInstance[T](init :T) :Volatile[T] = new VolatileRef[T](init)
	protected override def newBoolInstance(init :Boolean) :Volatile[Boolean] = new VolatileBool(init)

	/** An unspecialized `Volatile` implementation overriding atomic mutator methods to compare the value
	  * using `eq`/`ne`, rather than `==`/`!=` as in `Volatile` (which would call `equals` on reference types,
	  * which we do not want).
	  */
    @SerialVersionUID(1L)
	private class VolatileRef[T](init :T) extends Volatile[T](init) with RefVolatileLike[T]

	/** Optimised implementation of `Volatile[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
    @SerialVersionUID(1L)
	private class VolatileBool(init :Boolean) extends Volatile[Boolean](init) with BoolVolatileLike

}
