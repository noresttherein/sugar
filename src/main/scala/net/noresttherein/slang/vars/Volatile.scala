package net.noresttherein.slang.vars


import net.noresttherein.slang.vars.InOut.{DefaultValue, SpecializedVars}




/** A boxed `@volatile` variable of type `T`. Volatile declaration allows it to be used concurrently, guaranteeing
  * that changes will become visible to other threads, although no operations are atomic.
  * This makes it useful primarily as assigned-once variables.
  * This includes flags such as 'terminate thread' or lazily assigned values, where all assignments
  * are assumed to change it to the same value. Volatile variables incur lower overhead than full
  * java monitor synchronization, particularly with multiple concurrent reads.
  * If atomicity of more complex operations is required however, use [[net.noresttherein.slang.vars.SyncVar SyncVar]]
  * or [[net.noresttherein.slang.vars.Atomic Atomic]] instead.
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Volatile[@specialized(SpecializedVars) T](init :T) extends InOut[T] with Serializable {
	@scala.volatile private[this] var x = init

	final override def value :T = x

	final override def value_=(value :T) :Unit = x = value

	/** Assigns a new value to this variable, returning a value it held at some point in the past.
	  * Note that this is '''not''' atomic: other assignments might have happened between reading
	  * the previous value and assigning the provided value.
	  */
	final override def ?=(newValue :T) :T = { val res = x; x = newValue; res }

}






/** Factory of boxed `@volatile` variables. */
object Volatile {

	/** Create a new volatile reference variable which can be shared by multiple units of code. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Volatile[T] = new Volatile(value)

	/** Create a new volatile reference variable which can be shared by multiple units of code. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Volatile[T] =
		new Volatile(default.value)


	@inline implicit def unboxVar[@specialized(SpecializedVars) T](vol :Volatile[T]) :T = vol.get

}
