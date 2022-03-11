package net.noresttherein.slang.vars

import net.noresttherein.slang.vars.InOut.{InOutOrdering, SpecializedVars}
import net.noresttherein.slang.vars.Opt.Got
import net.noresttherein.slang.witness.DefaultValue




/** A marker interface of `InOut` variables which contain a value at all times.
  * No method of this object, including those inherited and those defined only in subclasses,
  * will throw a [[NoSuchElementException]].
  * @author Marcin Mościcki
  */
trait Mutable[@specialized(SpecializedVars) T] extends InOut[T] {
	final override def isDefined = true
	override def ?      :Some[T] = Some(value)
	override def opt    :Got[T]  = Got(value)
	override def unsure :Sure[T] = Sure(value)
}




object Mutable {
	@inline def apply[@specialized(SpecializedVars) T](value :T) :Mutable[T] = Var(value)
	@inline def apply[@specialized(SpecializedVars) T :DefaultValue] :Mutable[T] = Var[T]

	implicit def MutableOrdering[V[X] <: Mutable[X], T :Ordering] :Ordering[V[T]] = new InOutOrdering[V, T]
}