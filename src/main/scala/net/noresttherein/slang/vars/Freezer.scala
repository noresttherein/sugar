package net.noresttherein.slang.vars

import net.noresttherein.slang
import net.noresttherein.slang.vars.InOut.{DefaultValue, SpecializedVars}




/** A boxed variable whose value can become frozen at some point, causing all future reads to return the same,
  * frozen value. Its life cycle consists of two phases: during the initialization phase, the variable can
  * be mutated and read just as an synchronized [[net.noresttherein.slang.vars.Var Var]] instance, with writes
  * using no synchronization,This phase is not thread-safe, with no guarantees concerning the ordering of operations
  * or their atomicity. Once the [[net.noresttherein.slang.vars.Freezer.freeze freeze()]] method is called by any thread,
  * a ''release fence'' is invoked, synchronizing its value between the memory and processor cache, and the read-only
  * phase begins. Any future updates to the variable will throw a `IllegalStateException` and the variable will become
  * from this point effectively immutable and thread safe.
  *
  * The difference between `Export[T]` and `Freezer[T]` lies in atomicity: in this class, only the frozen, second
  * phase is thread-safe, and an update might be lost during freezing (it will be neither visible by any future read
  * nor will it fail with an exception). The latter is fully thread-safe in both phases at the cost of more expensive
  * access in the initialization phase.
  * @see [[net.noresttherein.slang.vars.Export Export]]
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(1L)
sealed class Freezer[@specialized(SpecializedVars) T](init :T) extends InOut[T] with Serializable {
	private[this] var x = init
	private[this] var exported = false

	final override def value :T = x

	@throws[IllegalStateException]("if the variable is frozen.")
	final override def value_=(value :T) :Unit =
		if (exported)
			throw new IllegalStateException(s"Freezer($x) is frozen.")
		else
            x = value

	/** Returns the value of this variable if it is already frozen. */
	@throws[IllegalStateException]("if the variable is not frozen.")
	def frozen :T =
		if (exported) x
		else throw new IllegalStateException(s"Freezer($x) is not frozen.")

	/** Freezes the value of this variable, making it immutable. Future updates will throw an `IllegalStateException`.
	  * This method is not thread-safe; it should be made from the same thread as all mutations. After it returns
	  * however, all threads will read th
	  */
	final def freeze() :Unit = {
		exported = true
		slang.publishMutable()
	}

	/** Checks if this variable has been frozen and is now immutable. */
	final def isFrozen :Boolean = exported

}






/** Factory of boxed `Freezer` variables. */
object Freezer {

	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Freezer[T] = new Freezer(value)

	/** Create a new variable which can become frozen at some point, turning it into a thread-safe, immutable value. */
	@inline def apply[@specialized(SpecializedVars) T](implicit default :DefaultValue[T]) :Freezer[T] =
		new Freezer(default.value)



	@inline implicit def unboxVar[@specialized(SpecializedVars) T](vol :Freezer[T]) :T = vol.get

}

