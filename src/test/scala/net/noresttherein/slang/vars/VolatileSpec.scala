package net.noresttherein.slang.vars

import org.scalacheck.Properties



class VolatilePropsGroup(varName :String = "Volatile") extends InOutPropsGroup(varName) {
	override def newVar[@specialized(Var.SpecializedTypes) T](x :T) :Volatile[T] = Volatile(x)
}

class ErasedVolatilePropsGroup extends VolatilePropsGroup("Volatile<erased>") {
	override def newVar[@specialized(Var.SpecializedTypes) T](x :T) :Volatile[T] = erasedVar(x)

	def erasedVar[T](x :T) :Volatile[T] = Volatile(x)
}

/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object VolatileSpec extends Properties("vars.Volatile") {
	(new VolatilePropsGroup).includeIn(this)
	(new ErasedVolatilePropsGroup).includeIn(this)
}
