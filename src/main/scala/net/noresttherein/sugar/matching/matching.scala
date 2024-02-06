package net.noresttherein.sugar

import net.noresttherein.sugar


package object matching {
	private[matching] final val Ver = sugar.Ver
}

package matching {

	import net.noresttherein.sugar.vars.Ternary

	/** Combines two extractor patterns into a logical conjunction which matches only if both of the patterns
	  * match (and binds values to both patterns). Usage: `{ case P1(x) && P2(y) => ??? }`.
	  */
	@SerialVersionUID(Ver)
	object && {
		def unapply[T](x :T) :Some[(T, T)] = Some(x, x)
	}

	/** An extractor always returning `true`, allowing to specify case condition as a pattern, rather
	  * then a conditional expression following it.
	  * It may be used together with [[net.noresttherein.sugar.matching.&& &&]] in order to evaluate the following
	  * pattern only if the condition is true. This is useful if evaluating of the following extractor pattern
	  * is expensive or would otherwise result in an error.
	  * @example {{{
	  *   list match {
	  *       case If(list.length > 0) && head::tail =>
	  *       case _ =>
	  *   }
	  * }}}
	  */
	@SerialVersionUID(Ver)
	object If {
		def unapply(any :Any) :Ternary = Ternary.True
	}
}