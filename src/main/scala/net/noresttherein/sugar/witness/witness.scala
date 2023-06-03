package net.noresttherein.sugar



package object witness {
	final val Ver = 1L
}


package witness {

	/** A function class used for implicit conversions in order to force precedence of one definition over another,
	  * despite having the same argument and return types.
	  * Defining a conversion as `SpecificConversion[X, Y]` means it will be chosen (if in the implicit search scope,
	  * or important) over any competing implicit definition of a `X => Y`, or an implicit method
	  * with the same signature.
	  */
	trait SpecificConversion[-X, +Y] extends (X => Y)

	object SpecificConversion {
		/** Forces a SAM type promotion of a function literal `X => Y` to a `SpecificConversion[X, Y]` and immediately
		  * returns it.
		  */
		def apply[X, Y](conversion :SpecificConversion[X, Y]) :conversion.type = conversion

		/** Promotes a function to a `SpecificConversion[X, Y]`. */
		def wrap[X, Y](conversion :X => Y) :SpecificConversion[X, Y] = conversion(_)
	}
}
