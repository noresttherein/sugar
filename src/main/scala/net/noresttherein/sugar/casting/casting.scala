package net.noresttherein.sugar

import net.noresttherein.sugar


package object casting extends extensions {
	private[casting] final val Ver = sugar.Ver

	/** Casts `null` to `X`. */
	@inline def nullAs[X] :X = null.asInstanceOf[X]
}