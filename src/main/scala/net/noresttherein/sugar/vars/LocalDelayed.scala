package net.noresttherein.sugar.vars

import net.noresttherein.sugar.noSuch_!
import net.noresttherein.sugar.vars.InOut.SpecializedVars




/** A non thread safe, lazily evaluated value, for use in local blocks.
  * It is useful if a value is relatively expensive to compute, and is used in some, but not all,
  * cases in pattern matching, or several `if else if` branches.
  * @example {{{
  *              val poem = LocalLazy(() => writePoem)
  *              val today = Date()
  *              if (today is 14.Jan)
  *                 girlfriend.give(poem.get)
  *              else if (today is girlfriend.birthday)
  *                 girlfriend.give(buyFlowers)
  *              else if (today is girlfriend.anniversary)
  *                 girlfriend.give(poem.get)
  *          }}}
  */
trait LocalDelayed[@specialized(SpecializedVars) T] extends Delayed[T] {
	private var x :T = _
	private var evaluated = false
	override def isDefinite :Boolean = evaluated
	override def get :T = {
		if (!evaluated) {
			x = init()
			evaluated = true
		}
		x
	}
	override def value :T =
		if (!evaluated) noSuch_!("Unevaluated LocalLazy")
		else x

	protected def init() :T
}


@SerialVersionUID(Ver)
case object LocalDelayed {
	@inline def apply[T](v :LocalDelayed[T]) :LocalDelayed[T] = v

	def delay[@specialized(SpecializedVars) T](value: => T) :LocalDelayed[T] = () => value
}
