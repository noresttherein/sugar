package net.noresttherein.sugar.vars

import scala.Specializable.Args

import net.noresttherein.sugar.vars.AtomicOps.{BoolAtomicVar, RefAtomicVar}
import net.noresttherein.sugar.vars.InOut.{InOutOrdering, SpecializedVars}
import net.noresttherein.sugar.witness.DefaultValue




/** Atomic variables providing several 'test-and-set' operations with the semantics of `equals`.
  * This class is very similar to [[net.noresttherein.sugar.vars.Volatile Volatile]], and, in fact,
  * implementations for value types are equivalent. However, the latter compares reference values
  * in all atomic methods using referential identity (`eq`), while this class runs the full `equals`.
  * @define Ref `Atomic`
  * @define ref atomic value
  * @author Marcin Mościcki marcin@moscicki.net
  */
@SerialVersionUID(Ver)
sealed class Atomic[@specialized(SpecializedVars) T] private[vars] //private[vars] due to specialization visibility issues
	extends AtomicOps.AtomicVar[T] with Mutable[T] with Serializable
{
	/* The default AtomicOps.Var update/updateLeft/updateRight implementation actually uses ==,
	 * which translates to equals and is according to our declared semantics.
	 * It is only through extending AtomicOps.RefAtomicVar that other AtomicVars obtain the eq semantics
	 * So, all we need is to simply use the regular Atomic[Any] as the class for reference values.
	 */
	@volatile private[this] var x :T = _
	protected override def factory :Atomic.type = Atomic

	final override def value :T = x
	final override def value_=(newValue :T) :Unit = x = newValue

	override def testAndSet(expect :T, newValue :T) :Boolean =
		super.testAndSet(expect, newValue) || x == newValue

	private[vars] override def isSpecialized :Boolean = getClass == classOf[Atomic[Any]]
	override def mkString :String = mkString("Atomic")
}




/** Factory of [[net.noresttherein.sugar.vars.Atomic atomic]] variables.
  * @define ref atomic variable
  */
@SerialVersionUID(Ver)
case object Atomic extends AtomicFactory[Atomic] {
	/** Create a new $ref which can be shared by multiple threads. */
	override def apply[@specialized(SpecializedVars) T](init :T) :Atomic[T] = {
		val atomic = new Atomic[T]
		if (atomic.getClass == classOf[Any]) {
			atomic.value = init
			atomic
		} else newSpecific(init)
	}
	override def apply[@specialized(SpecializedVars) T :DefaultValue] :Atomic[T] = super.apply[T]

	implicit def AtomicOrdering[T :Ordering] :Ordering[Atomic[T]] = new InOutOrdering[Atomic, T]

	protected override def newInstance[@specialized(SpecializedVars) T](init :T) :Atomic[T] = {
		val res = new Atomic[T]
		res.value = init
		res
	}
	protected override def newBoolInstance(init :Boolean) :Atomic[Boolean] = {
		val res = new Bool
		res.value = init
		res
	}
	protected override def newRefInstance[T](init :T) :Atomic[T] = {
		val res = new Atomic[T]
		res.value = init
		res
	}

	/** Optimised implementation of `Atomic[Bool]` which enumerates all two possible results
	  * in accumulate/mutate methods.
	  */
	@SerialVersionUID(Ver)
	private class Bool extends Atomic[Boolean] with BoolAtomicVar
}

