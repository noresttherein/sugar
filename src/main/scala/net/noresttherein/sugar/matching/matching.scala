package net.noresttherein.sugar




package object matching {
	private[matching] final val Ver = 1L
}

package matching {
	/** Combines two extractor patterns into a logical conjunction which matches only if both of the patterns
	  * match (and binds values to both patterns). Usage: `{ case P1(x) && P2(y) => ??? }`.
	  */
	@SerialVersionUID(Ver)
	object && {
		def unapply[T](x :T) :Some[(T, T)] = Some(x, x)
	}
}