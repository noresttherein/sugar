package net.noresttherein.sugar.collections

import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}




class IndexOf private (private val toInt :Int) extends AnyVal {
	def toOpt :Opt[Int] = if (toInt >= 0) Got(toInt) else Lack
	def isFound :Boolean = toInt >= 0
}

object IndexOf {
	object Found {
		def apply(i :Int) :IndexOf =
			if (i < 0) throw new IllegalArgumentException("Negative index: " + i)
			else new IndexOf(i)

		@inline def unapply(result :IndexOf) :Opt[Int] = result.toOpt
	}

	object Successor {
		def apply(i :Int) :IndexOf =
			if (i < 0) throw new IllegalArgumentException("Negative index: " + i)
			else new IndexOf(-i - 1)

		def unapply(result :IndexOf) :Opt[Int] = if (result.toInt < 0) Got(-result.toInt - 1) else Lack
	}
}
