package net.noresttherein.sugar.vars

import net.noresttherein.sugar.vars.InOut.SpecializedVars
import org.scalacheck.Properties



class AtomicPropsGroup(varName :String = "Atomic") extends InOutPropsGroup(varName) {
	override def newVar[@specialized(SpecializedVars) T](x :T) :Atomic[T] = Atomic(x)
}

class ErasedAtomicPropsGroup extends AtomicPropsGroup("Atomic<erased>") {
	override def newVar[@specialized(SpecializedVars) T](x :T) :Atomic[T] = erasedVar(x)

	def erasedVar[T](x :T) :Atomic[T] = Atomic(x)
}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object AtomicSpec extends Properties("vars.Atomic") {
	(new AtomicPropsGroup).includeIn(this)
	(new ErasedAtomicPropsGroup).includeIn(this)

	val default = Atomic[Int]
}
